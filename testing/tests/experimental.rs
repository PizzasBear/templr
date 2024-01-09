use templr::{templ, Template};

fn hello() -> impl Template<Context> {
    templ! {
        #use children as children;

        #children;
    }
}

fn affoela() -> impl Template<Context> {
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
                x?={ Some("hello") }
                normal="arg"
            } else {
                x?={ false }
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
        #hello() {
            <cultura />
        }
        #affoela();
        #{
            #let x = 3;
            { x }
            #{
                #let x = 5;
                { x }
            }
            { x }
        }

        #let x;
        {
            x = 3;
            ""
        }
        { x } { x }

        <f />
    };

    println!("{}", _a.render(&Context {}).unwrap());

    // panic!("SHOW ME MY OUTPUT")
}
