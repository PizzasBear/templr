use templr::{templ, Template};

fn hello() -> impl Template {
    templ! {
        #use children as children;

        #children;
    }
}

fn affoela() -> impl Template {
    templ! {
        affoela
    }
}

fn main() {
    let _a = templ! {
        <hello
            x-on:click="hello"
            #if true { x?={false} } else { x?={ false } }
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
            And this is the next text &#29;
        </hello>
        #for _ in 0..2 {
            <namida />
        }
        #hello() {
            <cultura />
        }
        #affoela();
    };

    println!("{}", _a.render(&(), &()).unwrap());
}
