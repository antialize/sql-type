// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use sql_parse::{OptSpanned, Spanned, Update};

use crate::{
    SelectTypeColumn, Type,
    type_::BaseType,
    type_expression::{ExpressionFlags, type_expression},
    type_reference::type_reference,
    type_select::{SelectType, type_select_exprs},
    typer::{Typer, typer_stack},
};

pub(crate) fn type_update<'a>(
    typer: &mut Typer<'a, '_>,
    update: &Update<'a>,
) -> Option<SelectType<'a>> {
    let mut guard = typer_stack(
        typer,
        |t| core::mem::take(&mut t.reference_types),
        |t, v| t.reference_types = v,
    );
    let typer = &mut guard.typer;

    for f in &update.flags {
        match f {
            sql_parse::UpdateFlag::LowPriority(_) | sql_parse::UpdateFlag::Ignore(_) => (),
        }
    }

    for reference in &update.tables {
        type_reference(typer, reference, false);
    }

    for (key, value) in &update.set {
        let flags = ExpressionFlags::default();
        match key.as_slice() {
            [key] => {
                let mut cnt = 0;
                let mut t = None;
                for r in &typer.reference_types {
                    for c in &r.columns {
                        if c.0 == *key {
                            cnt += 1;
                            t = Some(c.clone());
                        }
                    }
                }
                if cnt > 1 {
                    type_expression(typer, value, flags, BaseType::Any);
                    let mut issue = typer
                        .issues
                        .err("Ambiguous reference", &key.opt_span().unwrap());
                    for r in &typer.reference_types {
                        for c in &r.columns {
                            if c.0 == *key {
                                issue.frag("Defined here", &r.span);
                            }
                        }
                    }
                } else if let Some(t) = t {
                    let value_type = type_expression(typer, value, flags, t.1.base());
                    if typer.matched_type(&value_type, &t.1).is_none() {
                        typer.err(
                            alloc::format!("Got type {} expected {}", value_type, t.1),
                            value,
                        );
                    } else if let Type::Args(_, args) = &value_type.t {
                        for (idx, arg_type, _) in args.iter() {
                            typer.constrain_arg(*idx, arg_type, &t.1);
                        }
                    }
                } else {
                    type_expression(typer, value, flags, BaseType::Any);
                    typer
                        .issues
                        .err("Unknown identifier", &key.opt_span().unwrap());
                }
            }
            [table, column] => {
                let mut t = None;
                for r in &typer.reference_types {
                    if r.name != Some(table.clone()) {
                        continue;
                    }
                    for c in &r.columns {
                        if c.0 == column.clone() {
                            t = Some(c.clone());
                        }
                    }
                }
                if let Some(t) = t {
                    let value_type = type_expression(typer, value, flags, t.1.base());
                    if typer.matched_type(&value_type, &t.1).is_none() {
                        typer.err(
                            alloc::format!("Got type {} expected {}", value_type, t.1),
                            value,
                        );
                    } else if let Type::Args(_, args) = &value_type.t {
                        for (idx, arg_type, _) in args.iter() {
                            typer.constrain_arg(*idx, arg_type, &t.1);
                        }
                    }
                } else {
                    type_expression(typer, value, flags, BaseType::Any);
                    typer
                        .issues
                        .err("Unknown identifier", &key.opt_span().unwrap());
                }
            }
            _ => {
                type_expression(typer, value, flags, BaseType::Any);
                typer
                    .issues
                    .err("Unknown identifier", &key.opt_span().unwrap());
            }
        }
    }

    if let Some((where_, _)) = &update.where_ {
        let t = type_expression(typer, where_, ExpressionFlags::default(), BaseType::Bool);
        typer.ensure_base(where_, &t, BaseType::Bool);
    }

    
    match &update.returning {
        Some((returning_span, returning_exprs)) => {
            let columns = type_select_exprs(typer, returning_exprs, true)
                .into_iter()
                .map(|(name, type_, span)| SelectTypeColumn { name, type_, span })
                .collect();
            Some(SelectType {
                columns,
                select_span: returning_span.join_span(returning_exprs),
            })
        }
        None => None,
    }
}
