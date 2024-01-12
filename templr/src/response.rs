use std::fmt;

use crate::{Error, FnTemplate, Result, Template, ToTemplate};

pub struct Response(pub Result<String>);

pub trait TemplateExt<Ctx = ()>: ToTemplate<Ctx> {
    fn response(&self, ctx: &Ctx) -> Response {
        Response(self.to_template().render(ctx))
    }
}
impl<Ctx, T: ToTemplate<Ctx>> TemplateExt<Ctx> for T {}

use Response as TemplrResp;

#[allow(dead_code)]
const HTML_MIME_TYPE: &str = "text/html";

#[cfg(feature = "axum")]
mod resp_axum {
    use axum_core::response::{IntoResponse, Response};
    use http::{header, HeaderValue, StatusCode};

    use super::*;

    impl IntoResponse for TemplrResp {
        fn into_response(self) -> Response {
            match self.0 {
                Ok(body) => IntoResponse::into_response((
                    [(
                        header::CONTENT_TYPE,
                        HeaderValue::from_static(HTML_MIME_TYPE),
                    )],
                    body,
                )),
                Err(_) => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
            }
        }
    }

    impl<F> IntoResponse for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()> + Send,
    {
        fn into_response(self) -> Response {
            self.response(&()).into_response()
        }
    }
}

#[cfg(feature = "actix-web")]
mod resp_actix_web {
    use actix_web::{
        body::BoxBody,
        http::{header::HeaderValue, StatusCode},
        HttpRequest, HttpResponse, HttpResponseBuilder, Responder, ResponseError,
    };

    use super::*;

    struct AnyhowErr(Error);
    impl fmt::Display for AnyhowErr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(&self.0, f)
        }
    }
    impl fmt::Debug for AnyhowErr {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&self.0, f)
        }
    }
    impl ResponseError for AnyhowErr {}

    impl Responder for TemplrResp {
        type Body = BoxBody;

        fn respond_to(self, _req: &HttpRequest) -> HttpResponse {
            match self.0 {
                Ok(body) => HttpResponseBuilder::new(StatusCode::OK)
                    .content_type(HeaderValue::from_static(HTML_MIME_TYPE))
                    .body(body),
                Err(err) => HttpResponse::from_error(AnyhowErr(err)),
            }
        }
    }

    impl<F> Responder for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        type Body = BoxBody;

        fn respond_to(self, req: &HttpRequest) -> HttpResponse {
            self.response(&()).respond_to(req)
        }
    }
}

#[cfg(feature = "hyper")]
mod resp_hyper {
    use hyper::{
        header::{self, HeaderValue},
        Response, StatusCode,
    };

    use super::*;

    impl From<TemplrResp> for Response<String> {
        fn from(slf: TemplrResp) -> Self {
            match slf.0 {
                Ok(body) => Response::builder()
                    .status(StatusCode::OK)
                    .header(
                        header::CONTENT_TYPE,
                        HeaderValue::from_static(HTML_MIME_TYPE),
                    )
                    .body(body.into())
                    .unwrap(),
                Err(_) => Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(Default::default())
                    .unwrap(),
            }
        }
    }

    impl<F> From<FnTemplate<F>> for Response<String>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        fn from(slf: FnTemplate<F>) -> Self {
            slf.response(&()).into()
        }
    }
}

#[cfg(feature = "warp")]
mod resp_warp {
    pub use warp::reply::{Reply, Response};
    use warp::{
        http::{self, header, StatusCode},
        hyper::Body,
    };

    use super::*;

    impl Reply for TemplrResp {
        fn into_response(self) -> Response {
            match self.0 {
                Ok(body) => http::Response::builder()
                    .status(StatusCode::OK)
                    .header(header::CONTENT_TYPE, HTML_MIME_TYPE)
                    .body(body.into()),
                Err(_) => http::Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(Body::empty()),
            }
            .unwrap()
        }
    }

    impl<F> Reply for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()> + Send,
    {
        fn into_response(self) -> Response {
            self.response(&()).into_response()
        }
    }
}

#[cfg(feature = "tide")]
mod resp_tide {
    pub use tide::{Body, Response};

    use super::*;

    impl From<TemplrResp> for Response {
        fn from(slf: TemplrResp) -> Self {
            match slf.0 {
                Ok(body) => {
                    let mut body = Body::from_string(body);
                    body.set_mime(HTML_MIME_TYPE);

                    let mut response = Response::new(200);
                    response.set_body(body);
                    response
                }
                Err(error) => {
                    let mut response = Response::new(500);
                    response.set_error(error);
                    response
                }
            }
        }
    }

    impl<F> From<FnTemplate<F>> for Response
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        fn from(slf: FnTemplate<F>) -> Self {
            slf.response(&()).into()
        }
    }
}

#[cfg(feature = "gotham")]
mod resp_gotham {
    use gotham::hyper::{
        header::{self, HeaderValue},
        Body, Response, StatusCode,
    };
    use gotham::{handler::IntoResponse, state::State};

    use super::*;

    impl IntoResponse for TemplrResp {
        fn into_response(self, _state: &State) -> Response<Body> {
            match self.0 {
                Ok(body) => Response::builder()
                    .status(StatusCode::OK)
                    .header(
                        header::CONTENT_TYPE,
                        HeaderValue::from_static(HTML_MIME_TYPE),
                    )
                    .body(body.into())
                    .unwrap(),
                Err(_) => Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(vec![].into())
                    .unwrap(),
            }
        }
    }

    impl<F> IntoResponse for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        fn into_response(self, state: &State) -> Response<Body> {
            self.response(&()).into_response(state)
        }
    }
}

#[cfg(feature = "rocket")]
mod resp_rocket {
    use std::io::Cursor;

    use rocket::{
        http::{Header, Status},
        response::Response,
    };
    pub use rocket::{
        response::{Responder, Result},
        Request,
    };

    use super::*;

    impl<'r, 'o: 'r> Responder<'r, 'o> for TemplrResp {
        fn respond_to(self, _req: &'r Request<'_>) -> Result<'o> {
            let body = self.0.map_err(|_| Status::InternalServerError)?;
            Response::build()
                .header(Header::new("content-type", HTML_MIME_TYPE))
                .sized_body(body.len(), Cursor::new(body))
                .ok()
        }
    }

    impl<'r, 'o: 'r, F> Responder<'r, 'o> for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        fn respond_to(self, req: &'r Request<'_>) -> Result<'o> {
            self.response(&()).respond_to(req)
        }
    }
}
