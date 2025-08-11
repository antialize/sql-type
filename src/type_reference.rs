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

use crate::{
    schema::IndexKey,
    type_::BaseType,
    type_expression::{ExpressionFlags, type_expression},
    type_select::type_union_select,
    typer::{ReferenceType, Typer, unqualified_name},
};
use alloc::vec::Vec;
use sql_parse::{OptSpanned, Spanned, TableReference, issue_todo};

pub(crate) fn type_reference<'a>(
    typer: &mut Typer<'a, '_>,
    reference: &TableReference<'a>,
    force_null: bool,
) {
    let mut given_refs = core::mem::take(&mut typer.reference_types);
    match reference {
        sql_parse::TableReference::Table {
            identifier,
            as_,
            index_hints,
            ..
        } => {
            let identifier = unqualified_name(typer.issues, identifier);
            if let Some(s) = typer.get_schema(identifier.value) {
                let mut columns = Vec::new();
                for c in &s.columns {
                    let mut type_ = c.type_.clone();
                    type_.not_null = type_.not_null && !force_null;
                    columns.push((c.identifier.clone(), type_));
                }
                let name = as_.as_ref().unwrap_or(identifier).clone();
                for v in &typer.reference_types {
                    if v.name == Some(name.clone()) {
                        typer
                            .issues
                            .err("Duplicate definitions", &name)
                            .frag("Already defined here", &v.span);
                    }
                }
                for index_hint in index_hints {
                    if matches!(index_hint.type_, sql_parse::IndexHintType::Index(_)) {
                        for index in &index_hint.index_list {
                            if !typer.schemas.indices.contains_key(&IndexKey {
                                table: Some(identifier.clone()),
                                index: index.clone(),
                            }) {
                                typer.err("Unknown index", index);
                            }
                        }
                    }
                }

                typer.reference_types.push(ReferenceType {
                    name: Some(name.clone()),
                    span: name.span(),
                    columns,
                });
            } else {
                typer.issues.err("Unknown table or view", identifier);
            }
        }
        sql_parse::TableReference::Query { query, as_, .. } => {
            let select = type_union_select(typer, query, true);

            let span = if let Some(as_) = as_ {
                as_.span.clone()
            } else {
                select.columns.opt_span().unwrap_or_else(|| query.span())
            };

            typer.reference_types.push(ReferenceType {
                name: as_.clone(),
                span,
                columns: select
                    .columns
                    .iter()
                    .filter_map(|v| v.name.as_ref().map(|name| (name.clone(), v.type_.clone())))
                    .collect(),
            });
        }
        sql_parse::TableReference::Join {
            join,
            left,
            right,
            specification,
        } => {
            let (left_force_null, right_force_null) = match join {
                sql_parse::JoinType::Left(_) => (force_null, true),
                sql_parse::JoinType::Right(_) => (true, force_null),
                sql_parse::JoinType::Inner(_)
                | sql_parse::JoinType::Cross(_)
                | sql_parse::JoinType::Normal(_) => (force_null, force_null),
                _ => {
                    issue_todo!(typer.issues, join);
                    (force_null, force_null)
                }
            };
            type_reference(typer, left, left_force_null);
            type_reference(typer, right, right_force_null);
            match &specification {
                Some(sql_parse::JoinSpecification::On(e, _)) => {
                    let t = type_expression(typer, e, ExpressionFlags::default(), BaseType::Bool);
                    typer.ensure_base(e, &t, BaseType::Bool);
                }
                Some(s @ sql_parse::JoinSpecification::Using(_, _)) => {
                    issue_todo!(typer.issues, s);
                }
                None => (),
            }
        }
    }

    core::mem::swap(&mut typer.reference_types, &mut given_refs);
    typer.reference_types.extend(given_refs);
}
