use cfg_if::cfg_if;
use leptos::*;
mod counters;

// boilerplate to run in different modes
cfg_if! {
    // server-only stuff
    if #[cfg(feature = "ssr")] {
        use actix_files::{Files};
        use actix_web::*;
        use crate::counters::*;
        use leptos_actix::{generate_route_list, LeptosRoutes};

        #[get("/api/events")]
        async fn counter_events() -> impl Responder {
            use futures::StreamExt;

            let stream =
                futures::stream::once(async { crate::counters::get_server_count().await.unwrap_or(0) })
                    .chain(COUNT_CHANNEL.clone())
                    .map(|value| {
                        Ok(web::Bytes::from(format!(
                            "event: message\ndata: {value}\n\n"
                        ))) as Result<web::Bytes>
                    });
            HttpResponse::Ok()
                .insert_header(("Content-Type", "text/event-stream"))
                .streaming(stream)
        }

        #[actix_web::main]
        async fn main() -> std::io::Result<()> {

            crate::counters::register_server_functions();

            let conf = get_configuration(Some("Cargo.toml")).await.unwrap();
            let addr = conf.leptos_options.site_address.clone();
            let routes = generate_route_list(|cx| view! { cx, <Counters/> });

            HttpServer::new(move || {
                let leptos_options = &conf.leptos_options;
                let site_root = &leptos_options.site_root;

                App::new()
                    .service(counter_events)
                    .route("/api/{tail:.*}", leptos_actix::handle_server_fns())
                    .leptos_routes(leptos_options.to_owned(), routes.to_owned(), |cx| view! { cx, <Counters/> })
                    .service(Files::new("/", &site_root))
                    //.wrap(middleware::Compress::default())
            })
            .bind(&addr)?
            .run()
            .await
        }
        }

    // client-only stuff for Trunk
    else {
        use counter_isomorphic::counters::*;

        pub fn main() {
            _ = console_log::init_with_level(log::Level::Debug);
            console_error_panic_hook::set_once();
            mount_to_body(|cx| view! { cx, <Counter/> });
        }
    }
}
