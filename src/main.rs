extern crate rpds;

#[derive(Eq, PartialEq, Clone)]
#[allow(dead_code)]
enum Expression {
    Symbol(String),
    Vector(rpds::Vector<Expression>),
    Record(rpds::HashTrieMap<Expression, Expression>),
    Set(rpds::HashTrieSet<Expression>),
}

impl std::hash::Hash for Expression {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            &Expression::Symbol(ref string) => string.hash(state),
            &Expression::Vector(ref vector) => vector.hash(state),
            &Expression::Record(ref record) => record.iter().collect::<Vec<_>>().hash(state),
            &Expression::Set(ref set) => set.iter().collect::<Vec<_>>().hash(state),
        }
    }
}

fn to_string(expression: Expression) -> String {
    match expression {
        Expression::Vector(rpds_vec) => {
            let mut s = String::new();
            s.push_str("[");
            for e in rpds_vec.into_iter() {
                s.push_str(&to_string(e.clone()).as_str());
            }
            s.push_str("]");
            s
        },
        Expression::Symbol(symbol) => {
            let mut s = String::new();
            s.push_str("'");
            s.push_str(&symbol.as_str());
            s.push_str("'");
            s
        },
        Expression::Record(hashmap) => {
            let mut s = String::new();
            s.push_str("{");
            for (k, v) in hashmap.iter() {
                s.push_str("
");
                s.push_str(&to_string(k.clone()));
                s.push_str(&to_string(v.clone()));
            };
            s.push_str("}");
            s
        },
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
fn to_string_test () {
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

    assert_eq!(
        to_string(expression(&tokenize(&s("['one''two''three']")), &None, &None).0),
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

fn expression(
    tokens: &Vec<Token>,
    current_collection: &Option<Expression>,
    index_option: &Option<usize>
) -> (Expression, usize) {
    // returned usize is the index of tokens at time of returning
    match index_option {
        &None => expression(tokens, current_collection, &Some(0)),
        &Some(index) => {
            let current_token = tokens[index].clone();
            match current_token.token_type {
                TokenType::Symbol => match current_collection {
                    &Some(ref curr_coll) => {
                        match curr_coll.clone() {
                            Expression::Vector(rpds_vector) => expression(
                                tokens,
                                &Some(
                                    Expression::Vector(
                                        rpds_vector.push_back(
                                            Expression::Symbol(current_token.value)
                                        )
                                    )
                                ),
                                &Some(index + 1)),
                            Expression::Set(rpds_set) => expression(
                                tokens,
                                &Some(
                                    Expression::Set(
                                        rpds_set.insert(
                                            Expression::Symbol(current_token.value)
                                        )
                                    )
                                ),
                                &Some(index + 1)
                            ),
                            _ => panic!("Not done! current expression is {}", curr_coll)
                        }

                    },
                    &None => (Expression::Symbol(current_token.value), index)
                },
                TokenType::Record => {
                    // check if next token is }
                    // if so just return Expression::Record(rpds::HashTrieMap::new())
                    // else make a loop
                    // have a mutable usize `index_at_last_finish` initialized to `index + 1`
                    // have a mutable Vec `expressions`
                    // in the beginning check if `index_at_last_finish + 1` == "}"
                    // if so I am done
                    // Then the next step is to assemble the HashTrieMap by letting all odd indicies in expressions be keys, and even indicies be values
                    println!(
                        "
Starting creating a record. Starting at token: {}",
                        tokens[index].value.clone()
                    );
                    // what tokens index to start next expression from
                    let mut index_at_last_finish: usize = index + 1;
                    let mut expressions_buffer = vec![];
                    loop {
                        let current_token = tokens[index_at_last_finish].clone();
                        if current_token.value == s("}") {
                            // index_at_last_finish = index_at_last_finish - 1;
                            break;
                        } else {
                            let (current_expression, new_index) = expression(
                                tokens,
                                &None,
                                &Some(index_at_last_finish)
                            );

                            expressions_buffer.push(current_expression.clone());

                            index_at_last_finish = new_index + 1;
                            println!("Loop done, recurring time. current_expression: {}, index_at_last_finish: {}", current_expression.clone(), index_at_last_finish);
                        }
                    }
                    if expressions_buffer.len() == 1 {
                        println!("expressions_buffer[0] {}", expressions_buffer[0]);
                    }
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
                },
                TokenType::Vector => match current_collection {
                    &Some(ref curr_coll) => {
                        match curr_coll.clone() {
                            Expression::Vector(rpds_v) => {
                                if current_token.value == s("[") {
                                    let (child, child_index) = expression(
                                        tokens,
                                        &Some(
                                            Expression::Vector(rpds::Vector::new())
                                        ),
                                        &Some(index + 1)
                                    );
                                    (Expression::Vector(
                                        rpds_v.push_back(
                                            child
                                        )
                                    ), child_index)
                                } else if current_token.value == s("]") {
                                    (current_collection.clone().unwrap(), index)
                                } else {
                                    panic!("This should never happen!")
                                }
                            },
                            _ => panic!("Not done yet!")
                        }
                    },
                    &None => {
                        if current_token.value == s("[") {
                            expression(
                                tokens,
                                &Some(Expression::Vector(rpds::Vector::new())),
                                &Some(index + 1)
                            )
                        } else if current_token.value == s("]") {
                            (current_collection.clone().unwrap(), index)
                        } else {panic!("Invalid token {} for token type {:?}", current_token.value, TokenType::Vector)}
                    }
                },
                TokenType::Set => match current_collection {
                    &Some(ref curr_coll) => {
                        match curr_coll.clone() {
                            Expression::Set(rpds_set) => {
                                if current_token.value == s("<") {
                                    let (child, child_index) = expression(
                                        tokens,
                                        &Some(
                                            Expression::Set(rpds::HashTrieSet::new())
                                        ),
                                        &Some(index + 1)
                                    );
                                    (Expression::Set(
                                        rpds_set.insert(
                                            child
                                        )
                                    ), child_index)
                                } else if current_token.value == s(">") {
                                    (current_collection.clone().unwrap(), index)
                                } else {
                                    panic!("This should never happen!")
                                }
                            },
                            _ => panic!("Not done yet!")
                        }
                    },
                    &None => {
                        if current_token.value == s("<") {
                            expression(
                                tokens,
                                &Some(Expression::Set(rpds::HashTrieSet::new())),
                                &Some(index + 1)
                            )
                        } else if current_token.value == s(">") {
                            (current_collection.clone().unwrap(), index)
                        } else {panic!("Invalid token {} for token type {:?}", current_token.value, TokenType::Set)}
                    }
                }
            }
        }
    }
}

#[test]
fn expression_records_multiple_keyvals () {
    assert_eq!(
        expression(&tokenize(&s("{'key1' 'val1' 'key2' 'val2'}")),
                   &None,
                   &None),
        (Expression::Record(rpds::HashTrieMap::new().insert(
            Expression::Symbol(s("key1")),
            Expression::Symbol(s("val1"))
        ).insert(
            Expression::Symbol(s("key2")),
            Expression::Symbol(s("val2"))

        )), 5)
    )
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
               Expression::Vector(rpds::Vector::new()
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

fn main() {
    println!("{}", expression(&tokenize(&s("{'k' 'v'}")), &None, &None).0);
    println!("Hello, world!");

}
