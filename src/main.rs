extern crate rpds;

#[derive(Eq, PartialEq, Clone)]
#[allow(dead_code)]
enum Expression {
    Symbol(String),
    Vector(rpds::Vector<Expression>),
    Record(rpds::HashTrieMap<Expression, Expression>),
    Set(rpds::HashTrieSet<Expression>),
    Bool(bool),
}

impl std::hash::Hash for Expression {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            &Expression::Symbol(ref string) => string.hash(state),
            &Expression::Vector(ref vector) => vector.hash(state),
            &Expression::Record(ref record) => record.iter().collect::<Vec<_>>().hash(state),
            &Expression::Set(ref set) => set.iter().collect::<Vec<_>>().hash(state),
            &Expression::Bool(ref b) => b.hash(state),
        }
    }
}

fn to_string(expression: Expression) -> String {
    match expression {
        Expression::Bool(b) => {
            match b {
                true => String::from("T"),
                false => String::from("F"),
            }
        }
        Expression::Vector(rpds_vec) => {
            let mut s = String::new();
            s.push_str("[");
            for e in rpds_vec.into_iter() {
                s.push_str(&to_string(e.clone()).as_str());
            }
            s.push_str("]");
            s
        }
        Expression::Symbol(symbol) => {
            let mut s = String::new();
            s.push_str("'");
            s.push_str(&symbol.as_str());
            s.push_str("'");
            s
        }
        Expression::Record(hashmap) => {
            let mut s = String::new();
            s.push_str("{");
            for (k, v) in hashmap.iter() {
                s.push_str("
");
                s.push_str(&to_string(k.clone()));
                s.push_str(&to_string(v.clone()));
            }
            s.push_str("}");
            s
        }
        Expression::Set(hashset) => {
            let mut s = String::new();
            s.push_str("<");
            for e in hashset.iter() {
                s.push_str(&to_string(e.clone()).as_str());
            }
            s.push_str(">");
            s
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", to_string(self.clone()))
    }
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", to_string(self.clone()))
    }
}

#[test]
fn to_string_test() {
    assert_eq!(
        to_string(
            expression(
                &tokenize(
                    &to_string(
                        expression(
                            &tokenize(
                                &s("['one''two''three']")),
                            &None,
                            &None
                        ).0)),
                &None,
                &None
            ).0
        ),
        s("['one''two''three']"));

    assert_eq!(to_string(expression(&tokenize(&s("['one''two''three']")), &None, &None).0),
               s("['one''two''three']"));
}

fn s(s: &str) -> String {
    String::from(s)
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum TokenType {
    Symbol,
    Vector,
    Record,
    Set,
    Bool,
}

#[derive(Debug, Eq, PartialEq, Clone)]
struct Token {
    value: String,
    token_type: TokenType,
}

fn tokenize(source: &String) -> Vec<Token> {
    let mut to_ret: Vec<Token> = vec![];
    let mut am_i_building_a_symbol = false;
    let mut current_symbol_buffer: String = String::from("");
    let chars = source.chars();
    for c in chars {
        match c {
            '[' | ']' => {
                to_ret.push(Token {
                                value: c.to_string(),
                                token_type: TokenType::Vector,
                            })
            }
            '{' | '}' => {
                to_ret.push(Token {
                                value: c.to_string(),
                                token_type: TokenType::Record,
                            })
            }
            '<' | '>' => {
                to_ret.push(Token {
                                value: c.to_string(),
                                token_type: TokenType::Set,
                            })
            }
            'T' => {
                to_ret.push(Token {
                                value: c.to_string(),
                                token_type: TokenType::Bool,
                            })
            }
            'F' => {
                to_ret.push(Token {
                                value: c.to_string(),
                                token_type: TokenType::Bool,
                            })
            }
            '\'' => {
                if am_i_building_a_symbol {
                    am_i_building_a_symbol = false;
                    to_ret.push(Token {
                                    value: current_symbol_buffer,
                                    token_type: TokenType::Symbol,
                                });
                    current_symbol_buffer = String::from("");
                } else {
                    am_i_building_a_symbol = true;
                }
            }
            _ => {
                if am_i_building_a_symbol {
                    current_symbol_buffer.push(c)
                } else if c == ' ' {
                } else {
                    panic!("I'm given a character I don't know how to tokenize {}", c)
                }
            }
        }
    }
    to_ret
}

#[test]
fn tokenize_empty_vector_test() {
    assert_eq!(tokenize(&s("[]")),
               vec![Token {
                        value: s("["),
                        token_type: TokenType::Vector,
                    },
                    Token {
                        value: s("]"),
                        token_type: TokenType::Vector,
                    }])

}
#[test]
fn tokenize_symbol_test() {
    assert_eq!(tokenize(&s("'symbol'")),
               vec![Token {
                        value: s("symbol"),
                        token_type: TokenType::Symbol,
                    }])
}

#[test]
fn tokenize_vector_of_symbols_test() {
    assert_eq!(tokenize(&s("['Symbol 1' 'Symbol 2']")),
               vec![Token {
                        value: s("["),
                        token_type: TokenType::Vector,
                    },
                    Token {
                        value: s("Symbol 1"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s("Symbol 2"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s("]"),
                        token_type: TokenType::Vector,
                    }])
}
#[test]
fn tokenize_record_test() {
    assert_eq!(tokenize(&s("{['s1'] 'symsym'}")),
               vec![Token {
                        value: s("{"),
                        token_type: TokenType::Record,
                    },
                    Token {
                        value: s("["),
                        token_type: TokenType::Vector,
                    },
                    Token {
                        value: s("s1"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s("]"),
                        token_type: TokenType::Vector,
                    },
                    Token {
                        value: s("symsym"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s("}"),
                        token_type: TokenType::Record,
                    }])
}

#[test]
fn tokenize_set_test() {
    assert_eq!(tokenize(&s("<>")),
               vec![Token {
                        value: s("<"),
                        token_type: TokenType::Set,
                    },
                    Token {
                        value: s(">"),
                        token_type: TokenType::Set,
                    }]);
    assert_eq!(tokenize(&s("<'a' 'b' 'c'>")),
               vec![Token {
                        value: s("<"),
                        token_type: TokenType::Set,
                    },
                    Token {
                        value: s("a"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s("b"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s("c"),
                        token_type: TokenType::Symbol,
                    },
                    Token {
                        value: s(">"),
                        token_type: TokenType::Set,
                    }])
}

fn expression(tokens: &Vec<Token>,
              current_collection: &Option<Expression>,
              index_option: &Option<usize>)
              -> (Expression, usize) {
    // returned usize is the index of tokens at time of returning
    match index_option {
        &None => expression(tokens, current_collection, &Some(0)),
        &Some(index) => {
            let current_token = tokens[index].clone();
            match current_token.token_type {
                TokenType::Bool => {
                    if current_token.value == s("T") {
                        (Expression::Bool(true), index)
                    } else {
                        (Expression::Bool(false), index)
                    }
                }
                TokenType::Symbol => {
                    match current_collection {
                        &Some(ref _curr_coll) => panic!("I think this function arg is not required"),
                        // {
                        //     match curr_coll.clone() {
                        //         Expression::Vector(rpds_vector) => expression(
                        //             tokens,
                        //             &Some(
                        //                 Expression::Vector(
                        //                     rpds_vector.push_back(
                        //                         Expression::Symbol(current_token.value)
                        //                     )
                        //                 )
                        //             ),
                        //             &Some(index + 1)),
                        //         Expression::Set(rpds_set) => expression(
                        //             tokens,
                        //             &Some(
                        //                 Expression::Set(
                        //                     rpds_set.insert(
                        //                         Expression::Symbol(current_token.value)
                        //                     )
                        //                 )
                        //             ),
                        //             &Some(index + 1)
                        //         ),
                        //         _ => panic!("Not done! current expression is {}", curr_coll),
                        //     }
                        // }
                        &None => (Expression::Symbol(current_token.value), index),
                    }
                }
                TokenType::Record => {
                    // check if next token is }
                    // if so just return Expression::Record(rpds::HashTrieMap::new())
                    // else make a loop
                    // have a mutable usize `index_at_last_finish` initialized to `index + 1`
                    // have a mutable Vec `expressions`
                    // in the beginning check if `index_at_last_finish + 1` == "}"
                    // if so I am done
                    // Then the next step is to assemble the HashTrieMap by letting
                    // all odd indicies in expressions be keys, and even indicies be values
                    // what tokens index to start next expression from
                    let mut index_at_last_finish: usize = index + 1;
                    let mut expressions_buffer = vec![];
                    loop {
                        let current_token = tokens[index_at_last_finish].clone();
                        if current_token.value == s("}") {
                            // index_at_last_finish = index_at_last_finish - 1;
                            break;
                        } else {
                            let (current_expression, new_index) =
                                expression(tokens, &None, &Some(index_at_last_finish));

                            expressions_buffer.push(current_expression.clone());

                            index_at_last_finish = new_index + 1;
                        }
                    }
                    // Build the final data structure
                    let mut to_return = rpds::HashTrieMap::new();
                    loop {
                        if expressions_buffer.len() == 0 {
                            break;
                        } else {
                            let key = expressions_buffer[0].clone();
                            let value = expressions_buffer[1].clone();
                            expressions_buffer.remove(0);
                            if expressions_buffer.len() == 1 {
                                expressions_buffer = vec![]; // reset
                            } else {
                                expressions_buffer.remove(0);
                            }
                            to_return = to_return.insert(key, value)
                        }
                    }
                    (Expression::Record(to_return), index_at_last_finish)
                }
                TokenType::Vector => {
                    let mut index_at_last_finish: usize = index + 1;
                    let mut expressions = rpds::Vector::new();
                    loop {
                        let current_token = tokens[index_at_last_finish].clone();
                        if current_token.value == s("]") {
                            break;
                        } else {
                            let (current_expression, new_index) =
                                expression(tokens, &None, &Some(index_at_last_finish));
                            expressions = expressions.push_back(current_expression.clone());
                            index_at_last_finish = new_index + 1;
                        }
                    }
                    (Expression::Vector(expressions), index_at_last_finish)
                }
                TokenType::Set => {
                    let mut index_at_last_finish: usize = index + 1;
                    let mut expressions = rpds::HashTrieSet::new();
                    loop {
                        let current_token = tokens[index_at_last_finish].clone();
                        if current_token.value == s(">") {
                            break;
                        } else {
                            let (current_expression, new_index) =
                                expression(tokens, &None, &Some(index_at_last_finish));
                            expressions = expressions.insert(current_expression.clone());
                            index_at_last_finish = new_index + 1;
                        }
                    }
                    (Expression::Set(expressions), index_at_last_finish)
                }
            }
        }
    }
}

#[test]
fn expression_records_multiple_keyvals() {
    assert_eq!(expression(&tokenize(&s("{'key1' 'val1' 'key2' 'val2'}")), &None, &None),
               (Expression::Record(rpds::HashTrieMap::new()
                                       .insert(Expression::Symbol(s("key1")),
                                               Expression::Symbol(s("val1")))
                                       .insert(Expression::Symbol(s("key2")),
                                               Expression::Symbol(s("val2")))),
                5))
}

#[test]
fn expression_nested_records() {
    assert_eq!(tokenize(&s("{{} {}}")),
               vec![Token {
                        value: s("{"),
                        token_type: TokenType::Record,
                    },
                    Token {
                        value: s("{"),
                        token_type: TokenType::Record,
                    },
                    Token {
                        value: s("}"),
                        token_type: TokenType::Record,
                    },
                    Token {
                        value: s("{"),
                        token_type: TokenType::Record,
                    },
                    Token {
                        value: s("}"),
                        token_type: TokenType::Record,
                    },
                    Token {
                        value: s("}"),
                        token_type: TokenType::Record,
                    }]);
    assert_eq!(expression(&tokenize(&s("{{} {}}")), &None, &None),
               (Expression::Record(rpds::HashTrieMap::new()
                                       .insert(Expression::Record(rpds::HashTrieMap::new()),
                                               Expression::Record(rpds::HashTrieMap::new()))),
                5));

}

#[test]
fn expression_empty_record() {
    assert_eq!(expression(&tokenize(&s("{}")), &None, &None).0,
               Expression::Record(rpds::HashTrieMap::new()))
}

#[test]
fn expression_record_with_symbol_key() {
    assert_eq!(expression(&tokenize(&s("{'k' 'v'}")), &None, &None).0,
               Expression::Record(rpds::HashTrieMap::new().insert(Expression::Symbol(s("k")),
                                                                  Expression::Symbol(s("v")))))
}

#[test]
fn expression_record_with_key_val() {
    assert_eq!(
        expression(&tokenize(&s("{['quote' 'my key'] ['quote' 'value']}")), &None, &None).0,
        Expression::Record(
            rpds::HashTrieMap::new().insert(
                Expression::Vector(
                    rpds::Vector::new()
                        .push_back(
                            Expression::Symbol(
                                s("quote")))
                        .push_back(
                            Expression::Symbol(
                                s("my key"))))
                    ,
                Expression::Vector(
                    rpds::Vector::new()
                        .push_back(
                            Expression::Symbol(
                                s("quote")))
                        .push_back(
                            Expression::Symbol(
                                s("value"))))
            ))
    )
}

#[test]
fn expression_set() {
    assert_eq!(expression(&tokenize(&s("<>")), &None, &None).0,
               Expression::Set(rpds::HashTrieSet::new()))
}

#[test]
fn expression_set_of_symbols() {
    assert_eq!(expression(&tokenize(&s("<'one' 'two' 'one'>")), &None, &None).0,
               Expression::Set(rpds::HashTrieSet::new()
                                   .insert(Expression::Symbol(s("one")))
                                   .insert(Expression::Symbol(s("two")))))
}

#[test]
fn expression_nested_set() {
    assert_eq!(expression(&tokenize(&s("<'ett''två''tre'<'一''二'>>")), &None, &None).0,
               Expression::Set(rpds::HashTrieSet::new()
                               .insert(Expression::Symbol(s("ett")))
                               .insert(Expression::Symbol(s("två")))
                               .insert(Expression::Symbol(s("tre")))
                               .insert(Expression::Set(rpds::HashTrieSet::new()
                                                       .insert(Expression::Symbol(s("一")))
                                                       .insert(Expression::Symbol(s("二")))))))
}

#[test]
fn expression_double_vector() {
    assert_eq!(expression(&tokenize(&s("[[]]")), &None, &None).0,
               Expression::Vector(rpds::Vector::new()
                                      .push_back(Expression::Vector(rpds::Vector::new()))))
}

#[test]
fn expression_symbol_token_test() {
    assert_eq!(expression(&tokenize(&s("'symbol'")), &None, &None).0,
               Expression::Symbol(String::from("symbol")))
}

#[test]
fn expression_empty_vector_test() {
    assert_eq!(expression(&tokenize(&s("[]")), &None, &None).0,
               Expression::Vector(rpds::Vector::new()))
}

#[test]
fn expression_vector_of_symbols_test() {
    println!("tokenize(s(\"['one' 'two']\")) => {:?}",
             tokenize(&s("['one' 'two']")));
    assert_eq!(expression(&tokenize(&s("['one' 'two']")), &None, &None).0,
               Expression::Vector(rpds::Vector::new()
                                      .push_back(Expression::Symbol(String::from("one")))
                                      .push_back(Expression::Symbol(String::from("two")))))
}

#[test]
fn expression_nested_vector_of_symbols_test() {
    assert_eq!(expression(&tokenize(&s("['one' 'two' ['一' '二' ['三']]]")), &None, &None).0,
               Expression::Vector(
                   rpds::Vector::new()
                       .push_back(Expression::Symbol(String::from("one")))
                       .push_back(Expression::Symbol(String::from("two")))
                       .push_back(
                           Expression::Vector(
                               rpds::Vector::new()
                                   .push_back(
                                       Expression::Symbol(String::from("一")))
                                   .push_back(
                                       Expression::Symbol(String::from("二")))
                                   .push_back(Expression::Vector(
                                       rpds::Vector::new()
                                           .push_back(Expression::Symbol(String::from("三")))))))))
}

#[test]
fn expression_one_two_many_tests() {
    assert_eq!(
        expression(&tokenize(&s("['equals?' ['quote' 'yes'] ['quote' 'yes']]")), &None, &None).0,
        Expression::Vector(rpds::Vector::new()
                           .push_back(Expression::Symbol(s("equals?")))
                           .push_back(Expression::Vector(rpds::Vector::new()
                                                         .push_back(Expression::Symbol(s("quote")))
                                                         .push_back(Expression::Symbol(s("yes")))))
                           .push_back(Expression::Vector(rpds::Vector::new()
                                                         .push_back(Expression::Symbol(s("quote")))
                                                         .push_back(Expression::Symbol(s("yes"))))))
    )
}

fn evaluate(expression: Expression, context: Expression) -> Option<Expression> {
    let ctx: rpds::HashTrieMap<Expression, Expression> = match context {
        Expression::Record(ref r) => r.clone(),
        _ => panic!("Context must be Record"),
    };
    println!("Beginning evaluate");
    loop {
        match expression.clone() {
            Expression::Symbol(_string) => {
                match ctx.get(&expression) {
                    Some(e) => return Some(e.clone()),
                    None => return None,
                }
            }
            Expression::Vector(vec) => {
                let first = vec[0].clone();
                if first == Expression::Symbol(s("quote")) {
                    return Some(vec[1].clone());
                } else if first == Expression::Symbol(s("symbol?")) {
                    return Some(match evaluate(vec[1].clone(), context.clone()).unwrap() {
                                    Expression::Symbol(_sym) => Expression::Bool(true),
                                    _ => Expression::Bool(false),
                                });
                } else if first == Expression::Symbol(s("equals?")) {
                    println!("vec is {}", vec);
                    if vec[1] == vec[2] {
                        return Some(Expression::Bool(true));
                    } else {
                        return Some(Expression::Bool(false));
                    }
                } else if first == Expression::Symbol(s("first")) {
                    return evaluate(vec[1].clone(), context);
                } else if first == Expression::Symbol(s("rest")) {
                    let mut to_ret = rpds::Vector::new();
                    let mut index = 0;

                    println!("yoyo {}",
                             evaluate(vec[1].clone(), context.clone()).unwrap());

                    match evaluate(vec[1].clone(), context.clone()) {
                        Some(expr) => {
                            match expr {
                                Expression::Vector(rpds_v) => {
                                    for item in rpds_v.iter() {
                                        if index > 0 {
                                            to_ret = to_ret.push_back(item.clone());
                                        }
                                        index = index + 1;
                                    }
                                }
                                _ => return None,
                            }
                        }
                        None => return None,
                    }

                    return Some(Expression::Vector(to_ret));
                } else if first == Expression::Symbol(s("if")) {
                    if evaluate(vec[1].clone(), context.clone()).unwrap() ==
                       Expression::Bool(true) {
                        return evaluate(vec[2].clone(), context.clone());
                    } else if evaluate(vec[1].clone(), context.clone()).unwrap() ==
                              Expression::Bool(false) {
                        return evaluate(vec[3].clone(), context.clone());
                    }
                } else {
                    return None;
                }
            }
            Expression::Record(_rpds_hashtriemap) => panic!("l8r losers!"),
            Expression::Set(_rpds_hashtrieset) => panic!("l8r losers!"),
            Expression::Bool(_b) => return Some(expression),
        }
    }
}

fn eval_test(source_str: &str, context_source: &str, expectation: &str) {
    println!("Expression is: {}",
             expression(&tokenize(&s(source_str)), &None, &None).0);
    assert_eq!(evaluate(expression(&tokenize(&s(source_str)), &None, &None).0,
                        expression(&tokenize(&s(context_source)), &None, &None).0)
                       .unwrap(),
               expression(&tokenize(&s(expectation)), &None, &None).0);
}

#[test]
fn evaluate_quote_tests() {
    eval_test("['quote' 'プラトン']", "{}", "'プラトン'");
    eval_test("['quote' ['quote' 'プラトン']]",
              "{}",
              "['quote' 'プラトン']");
}

#[test]
fn evaluate_symbol_lookup_tests() {
    eval_test("'a'", "{'a' 'b'}", "'b'");
}

#[test]
fn evaluate_symbol_is_tests() {
    eval_test("['symbol?' ['quote' 'I am a symbol']]", "{}", "T");
    eval_test("['symbol?' ['quote' []]]", "{}", "F");
}

#[test]
fn evaluate_equals_tests() {
    eval_test("['equals?' ['quote' 'yes'] ['quote' 'yes']]", "{}", "T");
    eval_test("['equals?' ['quote' 'yes'] ['quote' 'no']]", "{}", "F");
    eval_test("['equals?' ['quote' 'no'] ['quote' 'yes']]", "{}", "F");
}

#[test]
fn evaluate_first_tests() {
    eval_test("['first' ['quote' 'a']]", "{}", "'a'");
}

#[test]
fn evaluate_rest_tests() {
    eval_test("['rest' ['quote' ['a' 'b' 'c']]]", "{}", "['b' 'c']");
}

#[test]
fn evaluate_push_tests() {
    eval_test("['push' ['quote' 'a'] ['quote' ['b']]]", "{}", "['a' 'b']");
}

#[test]
fn evaluate_bool_tests() {
    eval_test("T", "{}", "T");
    eval_test("F", "{}", "F");
}

#[test]
fn evaluate_if_tests() {
    eval_test("['if' T ['quote' 'I expect this'] ['quote' 'no']]",
              "{}",
              "'I expect this'");
    eval_test("['if' F ['quote' 'no'] ['quote' 'これの']]",
              "{}",
              "'これの'");
}

#[test]
fn evaluate_lambda_tests() {
    eval_test("[['lambda' ['x'] 'x'] ['quote' 'a']]", "{}", "'a'");

}

#[test]
fn evaluate_lambda_with_multiple_args() {
    eval_test("[['lambda' ['x' 'y'] ['push' 'x' 'y']] ['quote' 'a'] ['quote' 'b']]",
              "{}",
              "['a' 'b']");
}

#[test]
fn evaluate_lambda_from_context() {
    eval_test("['identity' ['quote' 'a']]",
              "{'identity' ['lambda' ['x'] 'x']}",
              "'a'");
}

#[test]
fn evaluate_higher_order_functions() {
    eval_test("[['lambda' ['x' 'f'] ['f' 'x']] ['quote' 'a'] ['lambda' ['x'] 'x']]",
              "{}",
              "'a'");
    eval_test("[['Get identity'] ['quote' 'a']]",
              "{'Get identity' ['lambda' [] ['lambda' ['x'] 'x']]}",
              "'a'");
}

#[test]
fn evaluate_the_id_of_something() {
    eval_test("['id'['quote''Thank you, Socrates, Plato, Alan Kay, Joe Armstrong, John McCarthy & Paul Graham. A point of view is worth 80 IQ points. ']]",
              "{}",
              "plato:0:something")
}

fn main() {
    println!("{}", expression(&tokenize(&s("{'k' 'v'}")), &None, &None).0);
    println!("Hello, world!");
}
