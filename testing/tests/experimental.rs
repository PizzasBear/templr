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
        <hello80.80.80
            x-on:click="hello"
            #if true { x?={Some("hello")} normal=arg } else { x?={ false } }
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
            {"This is text"}
            And this is the next text: "hello"
        </"hello80.80.80">
        #for _ in 0..2 {
            #break;
            <namida />
        }
        #hello() {
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
        {x = 3; ""}
        {x}
    };

    println!("{}", _a.render(&Context {}).unwrap());

    // panic!("SHOW ME MY OUTPUT")
}