use templr::{templ, Template};

#[test]
pub fn test_simple() {
    let t = templ! {
        <hello world this=yes and="true">This is some text</hello>
        {"string variable interpolation"}
    };

    assert_eq!(
        t.render(&()).unwrap().trim(),
        r#"<hello world this=yes and="true">This is some text</hello> string variable interpolation"#,
    );
}
