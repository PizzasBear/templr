#![allow(unused_imports, dead_code)]

use std::fmt;

use crate::{Error, FnTemplate, Result, Template};

/// A response struct that implements the various frameworks' response traits
pub struct Response(pub Result<String>);

/// Extension trait for generating a response
pub trait TemplateExt<Ctx: ?Sized = ()>: Template<Ctx> {
    /// Generates a `Response`
    fn response(self, ctx: &Ctx) -> Response;
}
impl<Ctx: ?Sized, T: Template<Ctx>> TemplateExt<Ctx> for T {
    fn response(self, ctx: &Ctx) -> TemplrResp {
        Response(self.render(ctx))
    }
}

use Response as TemplrResp;

const HTML_MIME_TYPE: &str = "text/html; charset=utf-8";

impl fmt::Display for TemplrResp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.0.as_ref().map_err(|_| fmt::Error)?, f)
    }
}

#[cfg(feature = "axum")]
mod resp_axum {
    use axum_core::response::{IntoResponse, Response};
    use http::{header, HeaderValue, StatusCode};

    use super::*;

    /// Axum support
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

    /// Axum support
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

    /// Actix Web support
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

    /// Actix Web support
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

    /// Hyper support
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

    /// Hyper support
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
    use warp::{
        http::{self, header, StatusCode},
        hyper::Body,
        reply::{Reply, Response},
    };

    use super::*;

    /// Warp support
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

    /// Warp support
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
    use tide::{Body, Response};

    use super::*;

    /// Tide support
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

    /// Tide support
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
    use gotham::{
        handler::IntoResponse,
        hyper::{
            header::{self, HeaderValue},
            Body, Response, StatusCode,
        },
        state::State,
    };

    use super::*;

    /// Gotham support
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

    /// Gotham support
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
        response::{Responder, Result},
        Request,
    };

    use super::*;

    /// Rocket support
    impl<'r, 'o: 'r> Responder<'r, 'o> for TemplrResp {
        fn respond_to(self, _req: &'r Request<'_>) -> Result<'o> {
            let body = self.0.map_err(|_| Status::InternalServerError)?;
            Response::build()
                .header(Header::new("content-type", HTML_MIME_TYPE))
                .sized_body(body.len(), Cursor::new(body))
                .ok()
        }
    }

    /// Rocket support
    impl<'r, 'o: 'r, F> Responder<'r, 'o> for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        fn respond_to(self, req: &'r Request<'_>) -> Result<'o> {
            self.response(&()).respond_to(req)
        }
    }
}

#[cfg(feature = "salvo")]
mod resp_salvo {
    use salvo_core::{
        http::{
            header::{self, HeaderValue},
            StatusCode,
        },
        Response, Scribe,
    };

    use super::*;

    /// Salvo support
    impl Scribe for TemplrResp {
        fn render(self, res: &mut Response) {
            match self.0 {
                Ok(body) => {
                    res.status_code(StatusCode::OK);
                    res.body(body);
                    res.headers_mut().insert(
                        header::CONTENT_TYPE,
                        HeaderValue::from_static(HTML_MIME_TYPE),
                    );
                }
                Err(_) => {
                    res.status_code(StatusCode::INTERNAL_SERVER_ERROR);
                }
            }
        }
    }

    /// Salvo support
    impl<F> Scribe for FnTemplate<F>
    where
        F: Fn(&mut dyn fmt::Write, &(), &dyn Template<()>) -> crate::Result<()>,
    {
        fn render(self, res: &mut Response) {
            self.response(&()).render(res)
        }
    }
}
