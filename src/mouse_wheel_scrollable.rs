//! Semantics for managing [`ViewportMutable`] [`Element`](super::element::Element)s that react to
//! mouse wheel events.

use super::{
    pointer_event_aware::PointerEventAware,
    raw::register_system,
    utils::{clone, spawn},
    viewport_mutable::ViewportMutable,
};
use apply::Apply;
use bevy_app::prelude::*;
use bevy_ecs::{prelude::*, system::SystemId};
use bevy_input::{mouse::*, prelude::*};
use bevy_ui::prelude::*;
use bevy_utils::prelude::*;
use futures_signals::signal::{BoxSignal, Mutable, Signal, SignalExt, always};
use haalka_futures_signals_ext::{SignalExtBool, StreamExtExt};
use std::{
    convert::Into,
    sync::{Arc, OnceLock},
};

type MouseWheelHandlerId = SystemId<In<(Entity, MouseWheel)>, ()>;
type MouseWheelHandlerSlot = Arc<OnceLock<MouseWheelHandlerId>>;

/// Marker [`Component`] that disables an element's viewport from reacting to mouse wheel events.
#[derive(Component, Default)]
pub struct ScrollDisabled;

/// Marker [`Component`] that enables an element's viewport to react to mouse wheel events.
#[derive(Component, Default)]
pub struct ScrollEnabled;

/// Stores registered mouse wheel handlers for an element.
#[derive(Component, Default)]
pub struct ScrollableMouseWheel {
    /// Registered handlers to run on mouse wheel input.
    pub handlers: Vec<MouseWheelHandlerSlot>,
}

/// Enables reacting to mouse wheel input on [`ViewportMutable`] elements.
pub trait MouseWheelScrollable: ViewportMutable {
    /// Register a system to run on mouse wheel events unless a `Disabled` component is present.
    fn on_scroll_with_system_disableable<Disabled: Component, Marker>(
        self,
        handler: impl IntoSystem<In<(Entity, MouseWheel)>, (), Marker> + Send + 'static,
    ) -> Self {
        self.update_raw_el(|raw_el| {
            let system_holder: MouseWheelHandlerSlot = Arc::new(OnceLock::new());
            raw_el
                .insert(ScrollEnabled)
                .insert(ScrollableMouseWheel::default())
                .on_spawn(clone!((system_holder) move |world, entity| {
                    let system = register_system(world, handler);
                    let _ = system_holder.set(system);
                    if let Some(mut scrollable) = world.get_mut::<ScrollableMouseWheel>(entity) {
                        scrollable.handlers.push(system_holder.clone());
                    }
                }))
        })
    }

    /// Register a system to run on mouse wheel events.
    fn on_scroll_with_system<Marker>(
        self,
        handler: impl IntoSystem<In<(Entity, MouseWheel)>, (), Marker> + Send + 'static,
    ) -> Self {
        self.on_scroll_with_system_disableable::<ScrollDisabled, Marker>(handler)
    }

    /// Register a system to run on mouse wheel events, enabling/disabling via a signal.
    fn on_scroll_with_system_disableable_signal<Marker>(
        self,
        handler: impl IntoSystem<In<(Entity, MouseWheel)>, (), Marker> + Send + 'static,
        blocked: impl Signal<Item = bool> + Send + 'static,
    ) -> Self {
        self.update_raw_el(|raw_el| raw_el.component_signal::<ScrollDisabled, _>(blocked.map_true(default)))
            .on_scroll_with_system_disableable::<ScrollDisabled, _>(handler)
    }

    /// Register a function to run on mouse wheel events unless a `Disabled` component is present.
    fn on_scroll_disableable<Disabled: Component>(
        self,
        mut handler: impl FnMut(MouseWheel) + Send + Sync + 'static,
    ) -> Self {
        self.on_scroll_with_system_disableable::<Disabled, _>(move |In((_, mouse_wheel))| handler(mouse_wheel))
    }

    /// Register a function to run on mouse wheel events.
    fn on_scroll(self, handler: impl FnMut(MouseWheel) + Send + Sync + 'static) -> Self {
        self.on_scroll_disableable::<ScrollDisabled>(handler)
    }

    /// Register a function to run on mouse wheel events, enabling/disabling via a signal.
    fn on_scroll_disableable_signal(
        self,
        handler: impl FnMut(MouseWheel) + Send + Sync + 'static,
        blocked: impl Signal<Item = bool> + Send + 'static,
    ) -> Self {
        self.update_raw_el(|raw_el| raw_el.component_signal::<ScrollDisabled, _>(blocked.map_true(default)))
            .on_scroll_disableable::<ScrollDisabled>(handler)
    }
}

/// Enables reacting to mouse wheel input only while the element is hovered.
pub trait OnHoverMouseWheelScrollable: MouseWheelScrollable + PointerEventAware {
    /// Register a system to run on mouse wheel events when hovered.
    fn on_scroll_with_system_on_hover<Marker>(
        self,
        handler: impl IntoSystem<In<(Entity, MouseWheel)>, (), Marker> + Send + 'static,
    ) -> Self {
        self.on_hovered_change_with_system(|In((entity, hovered)), mut commands: Commands| {
            if let Ok(mut entity) = commands.get_entity(entity) {
                if hovered {
                    entity.remove::<ScrollDisabled>();
                } else {
                    entity.try_insert(ScrollDisabled);
                }
            }
        })
        .on_scroll_with_system_disableable::<ScrollDisabled, _>(handler)
        .update_raw_el(|raw_el| raw_el.insert(ScrollDisabled))
    }

    /// Register a function to run on mouse wheel events when hovered.
    fn on_scroll_on_hover(self, mut handler: impl FnMut(MouseWheel) + Send + Sync + 'static) -> Self {
        self.on_scroll_with_system_on_hover::<_>(move |In((_, mouse_wheel))| handler(mouse_wheel))
    }
}

impl<T: PointerEventAware + MouseWheelScrollable> OnHoverMouseWheelScrollable for T {}

#[allow(missing_docs)]
#[derive(Clone, Copy, PartialEq)]
pub enum ScrollDirection {
    Horizontal,
    Vertical,
    Both,
}

/// Allows setting the direction and magnitude (in pixels) of viewport movement in response to mouse
/// wheel events. These settings can be either static or reactive via [`Signal`]s.
#[derive(Default)]
pub struct BasicScrollHandler {
    direction: Option<BoxSignal<'static, ScrollDirection>>,
    magnitude: Option<BoxSignal<'static, f32>>,
}

const DEFAULT_SCROLL_DIRECTION: ScrollDirection = ScrollDirection::Vertical;
const DEFAULT_SCROLL_MAGNITUDE: f32 = 10.;

/// Normalizes the scroll amount based on the scroll unit and the specified magnitude.
pub fn scroll_normalizer(unit: MouseScrollUnit, scroll: f32, magnitude: f32) -> f32 {
    match unit {
        MouseScrollUnit::Line => scroll * magnitude,
        MouseScrollUnit::Pixel => scroll.abs().min(magnitude) * scroll.signum(),
    }
}

impl BasicScrollHandler {
    #[allow(missing_docs)]
    pub fn new() -> Self {
        default()
    }

    /// Reactively set the [`ScrollDirection`] of viewport movement in response to mouse wheel
    /// events.
    pub fn direction_signal<S: Signal<Item = ScrollDirection> + Send + 'static>(
        mut self,
        direction_signal_option: impl Into<Option<S>>,
    ) -> Self {
        if let Some(direction_signal) = direction_signal_option.into() {
            self.direction = Some(direction_signal.boxed());
        }
        self
    }

    /// Set the [`ScrollDirection`] of viewport movement in response to mouse wheel events.
    pub fn direction(mut self, direction_option: impl Into<Option<ScrollDirection>>) -> Self {
        if let Some(direction) = direction_option.into() {
            self = self.direction_signal(always(direction));
        }
        self
    }

    /// Reactively set the magnitude (in pixels) of viewport movement in response to mouse wheel
    /// events.
    pub fn pixels_signal<S: Signal<Item = f32> + Send + 'static>(
        mut self,
        pixels_signal_option: impl Into<Option<S>>,
    ) -> Self {
        if let Some(pixels_signal) = pixels_signal_option.into() {
            self.magnitude = Some(pixels_signal.boxed());
        }
        self
    }

    /// Set the magnitude (in pixels) of viewport movement in response to mouse wheel events.
    pub fn pixels(mut self, pixels_option: impl Into<Option<f32>>) -> Self {
        if let Some(pixels) = pixels_option.into() {
            self = self.pixels_signal(always(pixels));
        }
        self
    }

    // TODO: is there a better return type for this ?
    /// Convert this [`BasicScrollHandler`] into a function that can be passed as a handler to
    /// `on_scroll_...` methods.
    #[allow(clippy::type_complexity)]
    pub fn into_system(
        self,
    ) -> Box<
        dyn FnMut(In<(Entity, MouseWheel)>, Res<ButtonInput<KeyCode>>, Query<&mut ScrollPosition>)
            + Send
            + Sync
            + 'static,
    > {
        let BasicScrollHandler {
            direction: direction_signal_option,
            magnitude: magnitude_signal_option,
        } = self;
        let direction = Mutable::new(DEFAULT_SCROLL_DIRECTION);
        let magnitude = Mutable::new(DEFAULT_SCROLL_MAGNITUDE);
        if let Some(direction_signal) = direction_signal_option {
            direction_signal
                .to_stream()
                .for_each_sync(clone!((direction) move |d| direction.set_neq(d)))
                .apply(spawn)
                .detach()
        }
        if let Some(magnitude_signal) = magnitude_signal_option {
            magnitude_signal
                .to_stream()
                .for_each_sync(clone!((magnitude) move |m| magnitude.set_neq(m)))
                .apply(spawn)
                .detach()
        }
        let f = move |In((entity, mouse_wheel)): In<(Entity, MouseWheel)>,
                      keys: Res<ButtonInput<KeyCode>>,
                      mut scroll_positions: Query<&mut ScrollPosition>| {
            let dy = scroll_normalizer(mouse_wheel.unit, mouse_wheel.y, magnitude.get());
            let direction = direction.get();
            if let Ok(mut scroll_position) = scroll_positions.get_mut(entity) {
                if matches!(direction, ScrollDirection::Vertical)
                    || matches!(direction, ScrollDirection::Both)
                        && !(keys.pressed(KeyCode::ShiftLeft) || keys.pressed(KeyCode::ShiftRight))
                {
                    scroll_position.y -= dy;
                } else if matches!(direction, ScrollDirection::Horizontal)
                    || matches!(direction, ScrollDirection::Both)
                        && (keys.pressed(KeyCode::ShiftLeft) || keys.pressed(KeyCode::ShiftRight))
                {
                    scroll_position.x -= dy;
                }
            }
        };
        Box::new(f)
    }
}

pub(super) fn plugin(_app: &mut App) {}
