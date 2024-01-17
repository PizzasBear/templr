use templr::{templ, templ_ret, Template, ToTemplate};

#[derive(Template)]
struct Named<'a> {
    name: &'a str,
}

impl ToTemplate<Context> for Named<'_> {
    fn to_template(&self) -> impl Template<Context> + '_ {
        templ! {
            Hello, {self.name}!
        }
    }
}

fn hello<'a>(named: &'a Named) -> templ_ret!['a, Context] {
    templ! {
        #use children;

        #children;
        #named;
    }
}

fn affoela() -> templ_ret![Context] {
    templ! {
        affoela
    }
}

struct Context {}

#[test]
fn experiment() {
    let _a = templ! {
        {"<script>alert(1)</script>"}
        <hello-80.80.80j
            x-on:click="hello"
            #if true {
                x?={Some("hello")}
                normal="arg"
            } else {
                x?={false}
            }
            @click="console.log(event)"
            #match true {
                true => {
                    this::is--true=yes
                }
                false => {
                    wrong=ok
                }
            }
        >
            {"This is text"} And this is the next text 55: "hello"
        </"hello-80.80.80j">
        #for _ in 0..2 {
            <namida />
        }
        #hello(&Named { name: "jo" }) {
            <cultura />
        }
        #affoela();
        #{
            #let x = 3;
            {x}
            #{
                #let x = 5;
                {x}
            }
            {x}
        }

        #let x;
        {
            x = 3;
            ""
        } {x}
        <f />
    };

    println!("{}", _a.render(&Context {}).unwrap());

    // panic!("SHOW ME MY OUTPUT")
}
