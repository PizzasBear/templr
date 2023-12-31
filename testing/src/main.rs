use templr::templ;

fn main() {
    let _a = templ! {
        <hello x-on:click="hello" #if true { x?={false} } else { x?={ false } } @click="console.log(event)">
            {"This is text"}
            And this is the next text &#29;
        </hello>
        #for x in 0..20 {
            <namida />
        }
    };
}
