# Graphics Examples

This section demonstrates ForGE Qt's 2D graphics capabilities, including drawing, animation, and visual effects.

## Basic Drawing

### Drawing Primitives

**File:** `examples/graphics_basic_drawing/graphics_basic_drawing.f90`

Demonstrates:
- Basic drawing operations
- Lines, rectangles, circles
- Coordinate systems
- Pen and brush settings

```fortran
type(forge_painter) :: painter
call painter%set_pen(forge_pen(color=black, width=2))
call painter%draw_line(0, 0, 100, 100)
```

### Shapes and Paths

**File:** `examples/graphics_shapes/graphics_shapes.f90`

Demonstrates:
- Complex shape drawing
- Bézier curves and paths
- Polygon rendering
- Shape composition

### Colors and Gradients

**File:** `examples/graphics_colors/graphics_colors.f90`

Demonstrates:
- Color specification and management
- RGB/HSV color models
- Gradient fills
- Transparency and alpha blending

## Text Rendering

### Text Drawing

**File:** `examples/graphics_text_rendering/graphics_text_rendering.f90`

Demonstrates:
- Font selection and configuration
- Text positioning and alignment
- Unicode text support
- Text formatting options

## Animation

### Basic Animation

**File:** `examples/graphics_animation_basic/graphics_animation_basic.f90`

Demonstrates:
- Object movement and transformation
- Animation timing
- Frame-based animation
- Collision detection

## Graphics Architecture

ForGE Qt's graphics system consists of:

1. **Graphics Scene:** Container for graphical items
2. **Graphics Items:** Individual drawable objects
3. **Graphics View:** Widget for displaying scenes
4. **Painter:** Low-level drawing API

### Scene Management

```fortran
type(forge_graphics_scene) :: scene
type(forge_graphics_view) :: view
type(forge_graphics_item) :: item

call scene%add_item(item)
call view%set_scene(scene)
```

### Item Types

- **Rectangles and Ellipses**
- **Lines and Curves**
- **Text Items**
- **Pixmap Items**
- **Custom Items**

## Coordinate Systems

ForGE Qt uses multiple coordinate systems:

- **Scene Coordinates:** Absolute positioning
- **View Coordinates:** Widget-relative positioning
- **Item Coordinates:** Local to each item

## Performance Optimization

1. **Bounding Rectangles:** Define update regions
2. **Item Caching:** Cache complex drawings
3. **Layered Rendering:** Optimize drawing order
4. **Dirty Region Tracking:** Minimize redraws

## Animation Framework

ForGE Qt provides animation support through:

- **Property Animation:** Animate object properties
- **Keyframe Animation:** Define animation paths
- **Group Animation:** Coordinate multiple animations
- **Easing Curves:** Control animation timing

```fortran
type(forge_property_animation) :: anim
call anim%set_target_object(my_item)
call anim%set_property_name("pos")
call anim%set_start_value([0.0, 0.0])
call anim%set_end_value([100.0, 100.0])
call anim%set_duration(1000)  ! 1 second
call anim%start()
```

## Custom Graphics Items

Create custom drawable objects:

```fortran
type, extends(forge_graphics_item) :: my_custom_item
contains
    procedure :: paint => my_paint
    procedure :: bounding_rect => my_bounding_rect
end type my_custom_item
```

## Running Graphics Examples

```bash
# Build and run graphics examples
cmake --build build --target graphics_basic_drawing
./build/examples/graphics_basic_drawing/graphics_basic_drawing

# Test animation
cmake --build build --target graphics_animation_basic
./build/examples/graphics_animation_basic/graphics_animation_basic
```

## Graphics Capabilities

| Feature | Examples | Description |
|---------|----------|-------------|
| **Drawing** | Basic Drawing, Shapes | 2D rendering primitives |
| **Colors** | Colors | Color and fill management |
| **Text** | Text Rendering | Font and text display |
| **Animation** | Animation Basic | Object animation |
| **Scenes** | All examples | Item management |

## Best Practices

1. **Efficient Painting:** Minimize drawing operations
2. **Proper Cleanup:** Release graphics resources
3. **Thread Safety:** Graphics operations are not thread-safe
4. **Caching:** Cache expensive drawing operations
5. **Clipping:** Use clipping regions for performance

These examples showcase ForGE Qt's comprehensive graphics capabilities for creating visually rich applications.