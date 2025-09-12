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
#![cfg_attr(not(test), no_std)]
#![forbid(unsafe_code)]

//! Crate for typing SQL statements.
//!
//! ```
//! use sql_type::{schema::parse_schemas, type_statement, TypeOptions,
//!     SQLDialect, SQLArguments, StatementType, Issues};
//! let schemas = "
//!     CREATE TABLE `events` (
//!       `id` bigint(20) NOT NULL,
//!       `user` int(11) NOT NULL,
//!       `message` text NOT NULL
//!     );";
//!
//! let mut issues = Issues::new(schemas);
//!
//! // Compute terse representation of the schemas
//! let schemas = parse_schemas(schemas,
//!     &mut issues,
//!     &TypeOptions::new().dialect(SQLDialect::MariaDB));
//! assert!(issues.is_ok());
//!
//! let sql = "SELECT `id`, `user`, `message` FROM `events` WHERE `id` = ?";
//! let mut issues = Issues::new(sql);
//! let stmt = type_statement(&schemas, sql, &mut issues,
//!     &TypeOptions::new().dialect(SQLDialect::MariaDB).arguments(SQLArguments::QuestionMark));
//! assert!(issues.is_ok());
//!
//! let stmt = match stmt {
//!     StatementType::Select{columns, arguments} => {
//!         assert_eq!(columns.len(), 3);
//!         assert_eq!(arguments.len(), 1);
//!     }
//!     _ => panic!("Expected select statement")
//! };
//! ```

extern crate alloc;

use alloc::vec::Vec;
use schema::Schemas;
pub use sql_parse::{Fragment, Issue, Issues, Level};
use sql_parse::{ParseOptions, parse_statement};

mod type_;
mod type_binary_expression;
mod type_delete;
mod type_expression;
mod type_function;
mod type_insert_replace;
mod type_reference;
mod type_select;
mod type_statement;
mod type_update;
mod typer;

pub mod schema;
pub use type_::{BaseType, FullType, Type};
pub use type_insert_replace::AutoIncrementId;
pub use type_select::SelectTypeColumn;
use typer::Typer;

pub use sql_parse::{SQLArguments, SQLDialect};

/// Options used when typing sql or parsing a schema
#[derive(Debug, Default, Clone)]
pub struct TypeOptions {
    parse_options: ParseOptions,
    warn_unnamed_column_in_select: bool,
    warn_duplicate_column_in_select: bool,
}

impl TypeOptions {
    /// Produce new default options
    pub fn new() -> Self {
        Default::default()
    }

    /// Change what sql dialect is used
    pub fn dialect(self, dialect: SQLDialect) -> Self {
        Self {
            parse_options: self.parse_options.dialect(dialect),
            ..self
        }
    }

    /// Change how sql arguments are supplied
    pub fn arguments(self, arguments: SQLArguments) -> Self {
        Self {
            parse_options: self.parse_options.arguments(arguments),
            ..self
        }
    }

    /// Should we warn about unquoted identifiers
    pub fn warn_unquoted_identifiers(self, warn_unquoted_identifiers: bool) -> Self {
        Self {
            parse_options: self
                .parse_options
                .warn_unquoted_identifiers(warn_unquoted_identifiers),
            ..self
        }
    }

    /// Should we warn about keywords not in ALL CAPS
    pub fn warn_none_capital_keywords(self, warn_none_capital_keywords: bool) -> Self {
        Self {
            parse_options: self
                .parse_options
                .warn_none_capital_keywords(warn_none_capital_keywords),
            ..self
        }
    }

    /// Should we warn about unnamed columns in selects
    pub fn warn_unnamed_column_in_select(self, warn_unnamed_column_in_select: bool) -> Self {
        Self {
            warn_unnamed_column_in_select,
            ..self
        }
    }

    /// Should we warn about duplicate columns in selects
    pub fn warn_duplicate_column_in_select(self, warn_duplicate_column_in_select: bool) -> Self {
        Self {
            warn_duplicate_column_in_select,
            ..self
        }
    }

    /// Parse _LIST_ as special expression and type as a list of items
    pub fn list_hack(self, list_hack: bool) -> Self {
        Self {
            parse_options: self.parse_options.list_hack(list_hack),
            ..self
        }
    }
}

/// Key of argument
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ArgumentKey<'a> {
    /// Index of unnamed argument
    Index(usize),
    /// Name of named argument
    Identifier(&'a str),
}

/// Type information of typed statement
#[derive(Debug, Clone)]
pub enum StatementType<'a> {
    /// The statement was a select statement
    Select {
        /// The types and named of the columns return from the select
        columns: Vec<SelectTypeColumn<'a>>,
        /// The key and type of arguments to the query
        arguments: Vec<(ArgumentKey<'a>, FullType<'a>)>,
    },
    /// The statement is a delete statement
    Delete {
        /// The key and type of arguments to the query
        arguments: Vec<(ArgumentKey<'a>, FullType<'a>)>,
        /// If present, the types and names of the columns returned from the delete
        returning: Option<Vec<SelectTypeColumn<'a>>>,
    },
    /// The statement is an insert statement
    Insert {
        /// The insert happend in a table with a auto increment id row
        yield_autoincrement: AutoIncrementId,
        /// The key and type of arguments to the query
        arguments: Vec<(ArgumentKey<'a>, FullType<'a>)>,
        /// If present, the types and names of the columns returned from the insert
        returning: Option<Vec<SelectTypeColumn<'a>>>,
    },
    /// The statement is a update statement
    Update {
        /// The key and type of arguments to the query
        arguments: Vec<(ArgumentKey<'a>, FullType<'a>)>,
        /// If present, the types and names of the columns returned from the insert
        returning: Option<Vec<SelectTypeColumn<'a>>>,
    },
    /// The statement is a replace statement
    Replace {
        /// The key and type of arguments to the query
        arguments: Vec<(ArgumentKey<'a>, FullType<'a>)>,
        /// If present, the types and names of the columns returned from the replace
        returning: Option<Vec<SelectTypeColumn<'a>>>,
    },
    /// The query was not valid, errors are preset in issues
    Invalid,
}

/// Type an sql statement with respect to a given schema
pub fn type_statement<'a>(
    schemas: &'a Schemas<'a>,
    statement: &'a str,
    issues: &mut Issues<'a>,
    options: &TypeOptions,
) -> StatementType<'a> {
    if let Some(stmt) = parse_statement(statement, issues, &options.parse_options) {
        let mut typer = Typer {
            schemas,
            issues,
            reference_types: Vec::new(),
            arg_types: Default::default(),
            options,
            with_schemas: Default::default(),
        };
        let t = type_statement::type_statement(&mut typer, &stmt);
        let arguments = typer.arg_types;
        match t {
            type_statement::InnerStatementType::Select(s) => StatementType::Select {
                columns: s.columns,
                arguments,
            },
            type_statement::InnerStatementType::Delete { returning } => StatementType::Delete {
                arguments,
                returning: returning.map(|r| r.columns),
            },
            type_statement::InnerStatementType::Insert {
                auto_increment_id,
                returning,
            } => StatementType::Insert {
                yield_autoincrement: auto_increment_id,
                arguments,
                returning: returning.map(|r| r.columns),
            },
            type_statement::InnerStatementType::Update { returning } => StatementType::Update {
                arguments,
                returning: returning.map(|r| r.columns),
            },
            type_statement::InnerStatementType::Replace { returning } => StatementType::Replace {
                arguments,
                returning: returning.map(|r| r.columns),
            },
            type_statement::InnerStatementType::Invalid => StatementType::Invalid,
        }
    } else {
        StatementType::Invalid
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec::Vec;
    use codespan_reporting::{
        diagnostic::{Diagnostic, Label},
        files::SimpleFiles,
        term::{
            self,
            termcolor::{ColorChoice, StandardStream},
        },
    };
    use sql_parse::{Identifier, Issue, Issues, Level, SQLArguments, SQLDialect};

    use crate::{
        ArgumentKey, AutoIncrementId, BaseType, FullType, SelectTypeColumn, StatementType, Type,
        TypeOptions, schema::parse_schemas, type_statement,
    };

    struct N<'a>(Option<&'a str>);
    impl<'a> alloc::fmt::Display for N<'a> {
        fn fmt(&self, f: &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
            if let Some(v) = self.0 {
                v.fmt(f)
            } else {
                f.write_str("None")
            }
        }
    }

    struct N2<'a>(Option<Identifier<'a>>);
    impl<'a> alloc::fmt::Display for N2<'a> {
        fn fmt(&self, f: &mut alloc::fmt::Formatter<'_>) -> alloc::fmt::Result {
            if let Some(v) = &self.0 {
                v.fmt(f)
            } else {
                f.write_str("None")
            }
        }
    }

    fn check_no_errors(name: &str, src: &str, issues: &[Issue], errors: &mut usize) {
        let mut files = SimpleFiles::new();
        let file_id = files.add(name, &src);
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        for issue in issues {
            let mut labels = vec![Label::primary(file_id, issue.span.clone())];
            for fragment in &issue.fragments {
                labels.push(
                    Label::secondary(file_id, fragment.span.clone())
                        .with_message(fragment.message.to_string()),
                );
            }
            let d = match issue.level {
                Level::Error => {
                    *errors += 1;
                    Diagnostic::error()
                }
                Level::Warning => Diagnostic::warning(),
            };
            let d = d
                .with_message(issue.message.to_string())
                .with_labels(labels);
            term::emit(&mut writer.lock(), &config, &files, &d).unwrap();
        }
    }

    fn str_to_type(t: &str) -> FullType<'static> {
        let (t, not_null) = if let Some(t) = t.strip_suffix('!') {
            (t, true)
        } else {
            (t, false)
        };
        let (t, list_hack) = if let Some(v) = t.strip_suffix("[]") {
            (v, true)
        } else {
            (t, false)
        };
        let t = match t {
            "b" => BaseType::Bool.into(),
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "i8" => Type::I8,
            "i16" => Type::I16,
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "i" => BaseType::Integer.into(),
            "f" => BaseType::Float.into(),
            "str" => BaseType::String.into(),
            "bytes" => BaseType::Bytes.into(),
            "dt" => BaseType::DateTime.into(),
            "json" => Type::JSON,
            "any" => BaseType::Any.into(),
            _ => panic!("Unknown type {t}"),
        };
        let mut t = FullType::new(t, not_null);
        if list_hack {
            t.list_hack = true;
        }
        t
    }

    fn check_arguments(
        name: &str,
        got: &[(ArgumentKey<'_>, FullType<'_>)],
        expected: &str,
        errors: &mut usize,
    ) {
        if expected.is_empty() {
            for (cnt, value) in got.iter().enumerate() {
                println!("{name}: Unexpected argument {cnt} type {value:?}");
                *errors += 1;
            }
            return;
        }
        let mut got2 = Vec::new();
        let inv = FullType::invalid();
        for (k, v) in got {
            match k {
                ArgumentKey::Index(i) => {
                    while got2.len() <= *i {
                        got2.push(&inv);
                    }
                    got2[*i] = v;
                }
                ArgumentKey::Identifier(k) => {
                    println!("{name}: Got named argument {k}");
                    *errors += 1;
                }
            }
        }
        let mut cnt = 0;
        for (i, t) in expected.split(',').enumerate() {
            let t = t.trim();
            let t = str_to_type(t);
            if let Some(v) = got2.get(i) {
                if *v != &t {
                    println!("{name}: Expected type {t} for argument {i} got {v}");
                    *errors += 1;
                }
            } else {
                println!("{name}: Expected type {t} for argument {i} got None");
                *errors += 1;
            }
            cnt += 1;
        }
        while cnt < got.len() {
            println!("{}: Unexpected argument {} type {:?}", name, cnt, got[cnt]);
            cnt += 1;
            *errors += 1;
        }
    }

    fn check_columns(name: &str, got: &[SelectTypeColumn<'_>], expected: &str, errors: &mut usize) {
        let mut cnt = 0;
        for (i, t) in expected.split(',').enumerate() {
            let t = t.trim();
            let (cname, t) = t.split_once(":").unwrap();
            let t = str_to_type(t);
            let cname = if cname.is_empty() { None } else { Some(cname) };
            if let Some(v) = got.get(i) {
                if v.name.as_deref() != cname || v.type_ != t {
                    println!(
                        "{}: Expected column {} with name {} of type {} got {} of type {}",
                        name,
                        i,
                        N(cname),
                        t,
                        N2(v.name.clone()),
                        v.type_
                    );
                    *errors += 1;
                }
            } else {
                println!(
                    "{}: Expected column {} with name {} of type {} got None",
                    name,
                    i,
                    N(cname),
                    t
                );
                *errors += 1;
            }
            cnt += 1;
        }
        while cnt < got.len() {
            println!(
                "{}: Unexpected column {} with name {} of type {}",
                name,
                cnt,
                N2(got[cnt].name.clone()),
                got[cnt].type_
            );
            cnt += 1;
            *errors += 1;
        }
    }

    #[test]
    fn mariadb() {
        let schema_src = "

        DROP TABLE IF EXISTS `t1`;
        CREATE TABLE `t1` (
          `id` int(11) NOT NULL,
          `cbool` tinyint(1) NOT NULL,
          `cu8` tinyint UNSIGNED NOT NULL,
          `cu16` smallint UNSIGNED NOT NULL,
          `cu32` int UNSIGNED NOT NULL,
          `cu64` bigint UNSIGNED NOT NULL,
          `ci8` tinyint,
          `ci16` smallint,
          `ci32` int,
          `ci64` bigint,
          `cbin` binary(16),
          `ctext` varchar(100) NOT NULL,
          `cbytes` blob,
          `cf32` float,
          `cf64` double,
          `cu8_plus_one` tinyint UNSIGNED GENERATED ALWAYS AS (
            `cu8` + 1
           ) STORED,
          `status` varchar(10) GENERATED ALWAYS AS (case when `cu8` <> 0 and `cu16` = 0 then 'a' when
            `cbool` then 'b' when `ci32` = 42 then 'd' when `cu64` = 43 then 'x' when
            `ci64` = 12 then 'y' else 'z' end) VIRTUAL
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8;

        ALTER TABLE `t1`
          MODIFY `id` int(11) NOT NULL AUTO_INCREMENT;

        DROP INDEX IF EXISTS `hat` ON `t1`;

        CREATE INDEX `hat2` ON `t1` (`id`, `cf64`);

        CREATE TABLE `t2` (
          `id` int(11) NOT NULL AUTO_INCREMENT,
          `t1_id` int(11) NOT NULL);

        CREATE TABLE `t3` (
            `id` int(11) NOT NULL AUTO_INCREMENT,
            `text` TEXT);

        CREATE TABLE `t4` (
            `id` int(11) NOT NULL AUTO_INCREMENT,
            `dt` datetime NOT NULL);

        CREATE TABLE `t5` (
            `id` int(11) NOT NULL AUTO_INCREMENT,
            `a` int NOT NULL,
            `b` int,
            `c` int NOT NULL DEFAULT 42);
        ";

        let options = TypeOptions::new().dialect(SQLDialect::MariaDB);
        let mut issues = Issues::new(schema_src);
        let schema = parse_schemas(schema_src, &mut issues, &options);
        let mut errors = 0;
        check_no_errors("schema", schema_src, issues.get(), &mut errors);

        let options = TypeOptions::new()
            .dialect(SQLDialect::MariaDB)
            .arguments(SQLArguments::QuestionMark);

        {
            let name = "q1";
            let src =
                "SELECT `id`, `cbool`, `cu8`, `cu8_plus_one`, `cu16`, `cu32`, `cu64`, `ci8`, `ci16`, `ci32`, `ci64`,
                `ctext`, `cbytes`, `cf32`, `cf64` FROM `t1` WHERE ci8 IS NOT NULL
                AND `cbool`=? AND `cu8`=? AND `cu16`=? AND `cu32`=? AND `cu64`=?
                AND `ci8`=? AND `ci16`=? AND `ci32`=? AND `ci64`=?
                AND `ctext`=? AND `cbytes`=? AND `cf32`=? AND `cf64`=?";

            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(
                    name,
                    &arguments,
                    "b,i,i,i,i,i,i,i,i,str,bytes,f,f",
                    &mut errors,
                );
                check_columns(
                    name,
                    &columns,
                    "id:i32!,cbool:b!,cu8:u8!,cu8_plus_one:u8!,cu16:u16!,cu32:u32!,cu64:u64!,
                    ci8:i8!,ci16:i16!,ci32:i32!,ci64:i64!,ctext:str!,cbytes:bytes!,cf32:f32!,cf64:f64!",
                    &mut errors,
                );
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q1.1";
            let src =
                "SELECT `id`, `cbool`, `cu8`, `cu16`, `cu32`, `cu64`, `ci8`, `ci16`, `ci32`, `ci64`,
                `ctext`, `cbytes`, `cf32`, `cf64`, `cbin` FROM `t1` WHERE ci8 IS NOT NULL";

            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(
                    name,
                    &columns,
                    "id:i32!,cbool:b!,cu8:u8!,cu16:u16!,cu32:u32!,cu64:u64!,
                    ci8:i8!,ci16:i16,ci32:i32,ci64:i64,ctext:str!,cbytes:bytes,cf32:f32,cf64:f64,cbin:bytes",
                    &mut errors,
                );
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q2";
            let src =
            "INSERT INTO `t1` (`cbool`, `cu8`, `cu16`, `cu32`, `cu64`, `ci8`, `ci16`, `ci32`, `ci64`,
            `ctext`, `cbytes`, `cf32`, `cf64`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";

            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Insert {
                arguments,
                yield_autoincrement,
                returning,
            } = q
            {
                check_arguments(
                    name,
                    &arguments,
                    "b!,u8!,u16!,u32!,u64!,i8,i16,i32,i64,str!,bytes,f32,f64",
                    &mut errors,
                );
                if yield_autoincrement != AutoIncrementId::Yes {
                    println!("{name} should yield autoincrement");
                    errors += 1;
                }
                if returning.is_some() {
                    println!("{name} should not return columns");
                    errors += 1;
                }
            } else {
                println!("{name} should be insert");
                errors += 1;
            }
        }

        {
            let name = "q3";
            let src =
                "DELETE `t1` FROM `t1`, `t2` WHERE `t1`.`id` = `t2`.`t1_id` AND `t2`.`id` = ?";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Delete { arguments, .. } = q {
                check_arguments(name, &arguments, "i", &mut errors);
            } else {
                println!("{name} should be delete");
                errors += 1;
            }
        }

        {
            let name = "q4";
            let src = "INSERT INTO `t2` (`t1_id`) VALUES (?) ON DUPLICATE KEY UPDATE `t1_id`=?";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Insert {
                arguments,
                yield_autoincrement,
                returning,
            } = q
            {
                check_arguments(name, &arguments, "i32!,i32!", &mut errors);
                if yield_autoincrement != AutoIncrementId::Optional {
                    println!("{name} should yield optional auto increment");
                    errors += 1;
                }
                if returning.is_some() {
                    println!("{name} should not return columns");
                    errors += 1;
                }
            } else {
                println!("{name} should be insert");
                errors += 1;
            }
        }

        {
            let name = "q5";
            let src = "INSERT IGNORE INTO `t2` SET `t1_id`=?";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Insert {
                arguments,
                yield_autoincrement,
                returning,
            } = q
            {
                check_arguments(name, &arguments, "i32!", &mut errors);
                if yield_autoincrement != AutoIncrementId::Optional {
                    println!("{name} should yield optional auto increment");
                    errors += 1;
                }
                if returning.is_some() {
                    println!("{name} should not return columns");
                    errors += 1;
                }
            } else {
                println!("{name} should be insert");
                errors += 1;
            }
        }

        {
            let name = "q6";
            let src = "SELECT IF(`ci32` IS NULL, `cbool`, ?) AS `cc` FROM `t1`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "b", &mut errors);
                check_columns(name, &columns, "cc:b", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q7";
            let src = "SELECT FROM_UNIXTIME(CAST(UNIX_TIMESTAMP() AS DOUBLE)) AS `cc` FROM `t1` WHERE `id`=?";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "i", &mut errors);
                check_columns(name, &columns, "cc:dt!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q8";
            let src = "REPLACE INTO `t2` SET `id` = ?, `t1_id`=?";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Replace {
                arguments,
                returning,
            } = q
            {
                check_arguments(name, &arguments, "i32!,i32!", &mut errors);
                if returning.is_some() {
                    println!("{name} should not return columns");
                    errors += 1;
                }
            } else {
                println!("{name} should be replace");
                errors += 1;
            }
        }

        {
            let name = "q9";
            let src = "INSERT INTO `t2` (`t1_id`) VALUES (32) ON DUPLICATE KEY UPDATE `t1_id` = `t1_id` + VALUES(`t1_id`)";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Insert { arguments, .. } = q {
                check_arguments(name, &arguments, "", &mut errors);
            } else {
                println!("{name} should be insert");
                errors += 1;
            }
        }

        {
            let name = "q10";
            let src =
                "SELECT SUBSTRING_INDEX(`text`, '/', 5) AS `k` FROM `t3` WHERE `text` LIKE '%T%'";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:str!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q11";
            let src = "SELECT * FROM `t1`, `t2` LEFT JOIN `t3` ON `t3`.`id` = `t1`.`id`";
            let mut issues: Issues<'_> = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if !issues.get().iter().any(|i| i.level == Level::Error) {
                println!("{name} should be an error");
                errors += 1;
            }
        }

        {
            let name = "q12";
            let src = "SELECT JSON_REPLACE('{ \"A\": 1, \"B\": [2, 3]}', '$.B[1]', 4, '$.C[3]', 3) AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:json", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let options = options.clone().list_hack(true);
            let name = "q13";
            let src = "SELECT `id` FROM `t1` WHERE `id` IN (_LIST_)";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "i[]", &mut errors);
                check_columns(name, &columns, "id:i32!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q14";
            let src = "SELECT CAST(NULL AS CHAR) AS `id`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "id:str", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q15";
            let src =
				"INSERT INTO `t1` (`cbool`, `cu8`, `cu16`, `cu32`, `cu64`, `ci8`, `ci16`, `ci32`, `ci64`,
            `ctext`, `cbytes`, `cf32`, `cf64`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                 RETURNING `id`, `cbool`, `cu8`, `ctext`, `cf64`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Insert {
                arguments,
                yield_autoincrement,
                returning,
            } = q
            {
                check_arguments(
                    name,
                    &arguments,
                    "b!,u8!,u16!,u32!,u64!,i8,i16,i32,i64,str!,bytes,f32,f64",
                    &mut errors,
                );
                if yield_autoincrement != AutoIncrementId::Yes {
                    println!("{name} should yield autoincrement");
                    errors += 1;
                }
                if let Some(returning) = returning {
                    check_columns(
                        name,
                        &returning,
                        "id:i32!,cbool:b!,cu8:u8!,ctext:str!,cf64:f64",
                        &mut errors,
                    );
                } else {
                    println!("{name} should return columns");
                    errors += 1;
                }
            } else {
                println!("{name} should be insert");
                errors += 1;
            }
        }

        {
            let name = "q16";
            let src = "REPLACE INTO `t2` SET `id` = ?, `t1_id`=? RETURNING `id`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Replace {
                arguments,
                returning,
            } = q
            {
                check_arguments(name, &arguments, "i32!,i32!", &mut errors);
                if let Some(returning) = returning {
                    check_columns(name, &returning, "id:i32!", &mut errors);
                } else {
                    println!("{name} should return columns");
                    errors += 1;
                }
            } else {
                println!("{name} should be replace");
                errors += 1;
            }
        }

        {
            let name = "q17";
            let src = "SELECT dt, UNIX_TIMESTAMP(dt) AS t FROM t4";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "dt:dt!,t:i64!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q17";
            let src = "SELECT CONCAT(?, \"hat\") AS c";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "any", &mut errors);
                check_columns(name, &columns, "c:str", &mut errors);
            } else {
                println!("{name} should be selsect");
                errors += 1;
            }
        }

        {
            let name = "q18";
            let src = "SELECT CAST(\"::0\" AS INET6) AS `id`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "id:str!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name: &str = "q18";
            let src = "SELECT SUBSTRING(`cbytes`, 1, 5) AS `k` FROM `t1`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:bytes", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q19";
            let src = "SELECT SUBSTRING(`ctext`, 1, 5) AS `k` FROM `t1`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:str!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q19";
            let src = "SELECT SUBSTRING(`ctext`, 1, 5) AS `k` FROM `t1`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:str!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q20";
            let src = "SELECT JSON_QUERY('{ \"A\": 1, \"B\": [2, 3]}', '$.B[1]') AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:json", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q21";
            let src = "SELECT JSON_REMOVE('{ \"A\": 1, \"B\": [2, 3]}', '$.B[1]', '$.C[3]') AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:json", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q22";
            let src = "SELECT JSON_OVERLAPS('false', 'false') AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:b!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q23";
            let src = "SELECT JSON_OVERLAPS('false', NULL) AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:b", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q24";
            let src = "SELECT JSON_CONTAINS('{\"A\": 0, \"B\": [\"x\", \"y\"]}', '\"x\"', '$.B') AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:b!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q25";
            let src = "SELECT JSON_CONTAINS('{\"A\": 0, \"B\": [\"x\", \"y\"]}', NULL, '$.A') AS `k` FROM `t3`";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "k:b", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q26";
            let src = "SELECT `id` FROM `t1` FORCE INDEX (`hat`)";
            let mut issues: Issues<'_> = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        {
            let name = "q27";
            let src = "SELECT `id` FROM `t1` USE INDEX (`hat2`)";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Select { arguments, columns } = q {
                check_arguments(name, &arguments, "", &mut errors);
                check_columns(name, &columns, "id:i32!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q28";
            let src = "INSERT INTO t5 (`a`) VALUES (44)";
            check_no_errors(name, src, issues.get(), &mut errors);
        }

        {
            let name = "q29";
            let src = "INSERT INTO t5 (`a`, `b`, `c`) VALUES (?, ?)";
            let mut issues: Issues<'_> = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        {
            let name = "q30";
            let src = "INSERT INTO t5 (`a`, `b`, `c`) VALUES (?, ?, ?)";
            check_no_errors(name, src, issues.get(), &mut errors);
        }

        {
            let name = "q31";
            let src = "INSERT INTO t5 (`a`, `b`, `c`) VALUES (?, ?, ?, ?)";
            let mut issues: Issues<'_> = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        {
            let name = "q32";
            let src = "INSERT INTO t5 (`b`, `c`) VALUES (44, 45)";
            let mut issues: Issues<'_> = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        if errors != 0 {
            panic!("{errors} errors in test");
        }
    }

    #[test]
    fn postgresql() {
        let schema_src = "
        BEGIN;

        DO $$ BEGIN
            CREATE TYPE my_enum AS ENUM (
            'V1',
            'V2',
            'V3'
        );
        EXCEPTION
            WHEN duplicate_object THEN null;
        END $$;

        CREATE TABLE IF NOT EXISTS t1 (
            id bigint NOT NULL PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
            path text NOT NULL UNIQUE,
            v my_enum NOT NULL,
            time timestamptz NOT NULL DEFAULT now(),
            old_id bigint,
            CONSTRAINT t1__old
            FOREIGN KEY(old_id) 
            REFERENCES t1(id)
            ON DELETE SET NULL
        );

        CREATE TABLE IF NOT EXISTS t2 (
            id bigint NOT NULL PRIMARY KEY
        );

        DROP INDEX IF EXISTS t2_index;

        CREATE INDEX t2_index2 ON t2 (id);

        CREATE TABLE IF NOT EXISTS t3 (
            a bigint NOT NULL,
            b bigint NOT NULL
        );

        CREATE UNIQUE INDEX t3u ON t3(a,b);

        COMMIT;
        ";

        let options = TypeOptions::new().dialect(SQLDialect::PostgreSQL);
        let mut issues = Issues::new(schema_src);
        let schema = parse_schemas(schema_src, &mut issues, &options);
        let mut errors = 0;
        check_no_errors("schema", schema_src, issues.get(), &mut errors);

        let options = TypeOptions::new()
            .dialect(SQLDialect::PostgreSQL)
            .arguments(SQLArguments::Dollar);

        {
            let name = "q1";
            let src = "INSERT INTO t2 (id) SELECT id FROM t1 WHERE path=$1 ON CONFLICT (id) DO NOTHING RETURNING id";
            let mut issues = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if let StatementType::Insert {
                arguments,
                returning,
                ..
            } = q
            {
                check_arguments(name, &arguments, "str", &mut errors);
                check_columns(name, &returning.expect("Returning"), "id:i64!", &mut errors);
            } else {
                println!("{name} should be select");
                errors += 1;
            }
        }

        {
            let name = "q2";
            let src = "WITH hat AS (DELETE FROM t1 WHERE old_id=42 RETURNING id) INSERT INTO t2 (id) SELECT id FROM hat";
            let mut issues = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);

            if let StatementType::Insert { arguments, .. } = q {
                check_arguments(name, &arguments, "", &mut errors);
            } else {
                println!("{name} should be select {q:?}");
                errors += 1;
            }
        }

        {
            let name = "q3";
            let src = "INSERT INTO t1 (path) VALUES ('HI')";
            let mut issues: Issues<'_> = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        {
            let name = "q4";
            let src = "INSERT INTO t1 (path, v) VALUES ('HI', 'V1')";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);

            if let StatementType::Insert { arguments, .. } = q {
                check_arguments(name, &arguments, "", &mut errors);
            } else {
                println!("{name} should be insert {q:?}");
                errors += 1;
            }
        }

        {
            let name = "q5";
            let src = "UPDATE t1 SET path='HI' RETURNING id";
            let mut issues: Issues<'_> = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            if let StatementType::Update {
                arguments,
                returning,
                ..
            } = q
            {
                check_arguments(name, &arguments, "", &mut errors);
                if returning.is_none() {
                    println!("{name} should have returning");
                    errors += 1;
                }
            } else {
                println!("{name} should be update {q:?}");
                errors += 1;
            }
        }

        {
            let name = "q6";
            let src = "INSERT INTO t3 (a,b) VALUES (1, 1) ON CONFLICT (a,b) DO UPDATE SET a=t3.a, b=EXCLUDED.b WHERE t3.a != EXCLUDED.a";
            let mut issues = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);

            if let StatementType::Insert { arguments, .. } = q {
                check_arguments(name, &arguments, "", &mut errors);
            } else {
                println!("{name} should be insert {q:?}");
                errors += 1;
            }
        }

        {
            let name = "q7";
            let src = "INSERT INTO t3 (a,b) VALUES (1, 1) ON CONFLICT (a,c) cake DO UPDATE SET a=2";
            let mut issues = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        {
            let name = "q8";
            let src = "INSERT INTO t3 (a,b) VALUES (1, 1) ON CONFLICT (a,b) t3u DO UPDATE SET a=2 WHERE b=2";
            let mut issues = Issues::new(src);
            type_statement(&schema, src, &mut issues, &options);
            if issues.is_ok() {
                println!("{name} should fail");
                errors += 1;
            }
        }

        if errors != 0 {
            panic!("{errors} errors in test");
        }
    }

    #[test]
    fn sqlite() {
        let schema_src = "
         CREATE TABLE IF NOT EXISTS `t1` (
            `id` INTEGER NOT NULL PRIMARY KEY,
            `sid` TEXT NOT NULL) STRICT;
        CREATE UNIQUE INDEX IF NOT EXISTS `t1_sid` ON `t1` (`sid`);
        ";

        let options = TypeOptions::new().dialect(SQLDialect::Sqlite);
        let mut issues = Issues::new(schema_src);
        let schema = parse_schemas(schema_src, &mut issues, &options);
        let mut errors = 0;
        check_no_errors("schema", schema_src, issues.get(), &mut errors);

        let options = TypeOptions::new()
            .dialect(SQLDialect::Sqlite)
            .arguments(SQLArguments::QuestionMark);

        {
            let name = "q1";
            let src = "INSERT INTO `t1` (`sid`) VALUES (?)";
            let mut issues = Issues::new(src);
            let q = type_statement(&schema, src, &mut issues, &options);
            check_no_errors(name, src, issues.get(), &mut errors);
            if !matches!(q, StatementType::Insert { .. }) {
                println!("{name} should be select");
                errors += 1;
            }
        }

        if errors != 0 {
            panic!("{errors} errors in test");
        }
    }
}
