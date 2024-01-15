use templr::{templ, Template};

#[test]
pub fn test_simple() {
    let t = templ! {
        <p>
            <hello world this=yes and="true">This is some text</hello>
        </p>
        {"string variable interpolation"}
    };

    assert_eq!(
        t.render(&()).unwrap().trim(),
        r#"<p><hello world this=yes and="true">This is some text</hello></p> string variable interpolation"#,
    );
}
