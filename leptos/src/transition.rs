use cfg_if::cfg_if;
use leptos_dom::{Component, DynChild, Fragment, IntoView};
use leptos_macro::component;
use leptos_reactive::{provide_context, Scope, SignalSetter, SuspenseContext};
use std::{cell::RefCell, rc::Rc};
#[cfg(not(any(feature = "csr", feature = "hydrate")))]
use leptos_dom::{HydrationCtx, HydrationKey};

/// If any [Resource](leptos_reactive::Resource)s are read in the `children` of this
/// component, it will show the `fallback` while they are loading. Once all are resolved,
/// it will render the `children`. Unlike [`Suspense`](crate::Suspense), this will not fall
/// back to the `fallback` state if there are further changes after the initial load.
///
/// Note that the `children` will be rendered initially (in order to capture the fact that
/// those resources are read under the suspense), so you cannot assume that resources have
/// `Some` value in `children`.
///
/// ```
/// # use leptos_reactive::*;
/// # use leptos_core::*;
/// # use leptos_macro::*;
/// # use leptos_dom::*; use leptos::*;
/// # run_scope(create_runtime(), |cx| {
/// # if cfg!(not(any(feature = "csr", feature = "hydrate", feature = "ssr"))) {
/// async fn fetch_cats(how_many: u32) -> Result<Vec<String>, ()> { Ok(vec![]) }
///
/// let (cat_count, set_cat_count) = create_signal::<u32>(cx, 1);
/// let (pending, set_pending) = create_signal(cx, false);
///
/// let cats = create_resource(cx, cat_count, |count| fetch_cats(count));
///
/// view! { cx,
///   <div>
///     <Transition
///       fallback=move || view! { cx, <p>"Loading..."</p>}
///       set_pending=set_pending
///     >
///       {move || {
///           cats.read().map(|data| match data {
///             Err(_) => view! { cx,  <pre>"Error"</pre> },
///             Ok(cats) => view! { cx,
///               <div>{
///                 cats.iter()
///                   .map(|src| {
///                     view! { cx,
///                       <img src={src}/>
///                     }
///                   })
///                   .collect::<Vec<_>>()
///               }</div>
///             },
///           })
///         }
///       }
///     </Transition>
///   </div>
/// };
/// # }
/// # });
/// ```
#[component(transparent)]
pub fn Transition<F, E>(
    cx: Scope,
    /// Will be displayed while resources are pending.
    fallback: F,
    /// A function that will be called when the component transitions into or out of
    /// the `pending` state, with its argument indicating whether it is pending (`true`)
    /// or not pending (`false`).
    #[prop(optional)]
    set_pending: Option<SignalSetter<bool>>,
    /// Will be displayed once all resources have resolved.
    children: Box<dyn Fn(Scope) -> Fragment>
) -> impl IntoView
where
    F: Fn() -> E + 'static,
    E: IntoView,
{
    let context = SuspenseContext::new(cx);

    // provide this SuspenseContext to any resources below it
    provide_context(cx, context);

    let orig_child = Rc::new(children);

    #[cfg(any(feature = "csr", feature = "hydrate"))]
    let prev_child = RefCell::new(None);

    Component::new("Transition", move |cx| {
        #[cfg(not(any(feature = "csr", feature = "hydrate")))]
        let current_id = HydrationCtx::peek();

        DynChild::new(move || {        
            cfg_if! {
                if #[cfg(any(feature = "csr", feature = "hydrate"))] {
                    if context.ready() {  
                        let current_child = orig_child(cx).into_view(cx);
                        *prev_child.borrow_mut() = Some(current_child.clone());
                        if let Some(pending) = &set_pending {
                            pending.set(false);
                        }
                        current_child
                    } else if let Some(prev_child) = &*prev_child.borrow() {
                        if let Some(pending) = &set_pending {
                            pending.set(true);
                        }
                        prev_child.clone()
                    } else {
                        if let Some(pending) = &set_pending {
                            pending.set(true);
                        }
                        let fallback = fallback().into_view(cx);
                        *prev_child.borrow_mut() = Some(fallback.clone());
                        fallback
                    }
                } else {
                    // run the child; we'll probably throw this away, but it will register resource reads
                    let child = orig_child(cx).into_view(cx);
            
                    let initial = {    
                        // no resources were read under this, so just return the child
                        if context.pending_resources.get() == 0 {
                            child.clone()
                        }
                        // show the fallback, but also prepare to stream HTML
                        else {
                            let orig_child = Rc::clone(&orig_child);
                            cx.register_suspense(context, &current_id.to_string(), {
                                let current_id = current_id.clone();
                                let fragment_id = HydrationKey {
                                    previous: current_id.previous,
                                    offset: current_id.offset + 1
                                };
                                move || {
                                    HydrationCtx::continue_from(fragment_id);
                                    orig_child(cx)
                                        .into_view(cx)
                                        .render_to_string(cx)
                                        .to_string()
                                }
                            });
                
                            // return the fallback for now, wrapped in fragment identifer
                            fallback().into_view(cx)
                        }
                    };
            
                    HydrationCtx::continue_from(current_id.clone());
            
                    initial
                }
            }
        })
    })
}