# Mobile Development Guide

This guide covers mobile development with ForGE, including iOS and Android support, mobile UI patterns, and mobile-specific development practices.

## Mobile Platform Support

### iOS Integration

ForGE provides iOS support through UIKit integration:

```fortran
use forge_platform_ios
use iso_c_binding

type(forge_ios_platform) :: platform
type(platform_window_handle) :: window_handle
type(forge_status) :: status

! Initialize iOS platform
call platform%init(status)

! Create UIViewController
call platform%create_window(window_handle, "My App", 375, 667, status)

! Access UIKit objects
uiviewcontroller = window_handle%ios_view_controller
uiview = window_handle%ios_view

! Use UIKit APIs directly
call objc_msgSend(uiviewcontroller, "setTitle:", "My App")
call objc_msgSend(uiview, "setBackgroundColor:", white_color)
```

### Android Integration

ForGE supports Android through JNI integration:

```fortran
use forge_platform_android
use iso_c_binding

type(forge_android_platform) :: platform
type(platform_window_handle) :: window_handle
type(forge_status) :: status

! Initialize Android platform
call platform%init(status)

! Create Activity
call platform%create_window(window_handle, "My App", 1080, 1920, status)

! Access Android objects
activity = window_handle%android_activity
view = window_handle%android_view

! Use Android APIs through JNI
call android_set_content_view(activity, view)
call android_set_title(activity, "My App")
```

## Mobile UI Patterns

### Responsive Design

```fortran
! Mobile-responsive layout
program responsive_mobile_app
    use forge_mobile_layout

    type(mobile_layout) :: layout
    type(screen_size) :: screen

    ! Get device screen size
    screen = get_device_screen_size()

    ! Create responsive layout
    if (screen%width < 600) then
        ! Phone layout
        call layout%set_orientation(LAYOUT_VERTICAL)
        call layout%set_spacing(8)
    else
        ! Tablet layout
        call layout%set_orientation(LAYOUT_HORIZONTAL)
        call layout%set_spacing(16)
    end if

    ! Add responsive widgets
    call layout%add_widget(header, FLEX_GROW)
    call layout%add_widget(content, FLEX_GROW * 3)
    call layout%add_widget(footer, FLEX_FIXED(50))

end program responsive_mobile_app
```

### Touch Interactions

```fortran
! Touch gesture handling
program touch_app
    use forge_mobile_touch

    type(touch_gesture_recognizer) :: tap_recognizer, swipe_recognizer

    ! Set up tap gesture
    call tap_recognizer%init(GESTURE_TAP)
    call tap_recognizer%set_number_of_taps_required(1)
    call tap_recognizer%set_callback(tap_handler)

    ! Set up swipe gesture
    call swipe_recognizer%init(GESTURE_SWIPE)
    call swipe_recognizer%set_direction(SWIPTE_DIRECTION_RIGHT)
    call swipe_recognizer%set_callback(swipe_handler)

    ! Add to view
    call view%add_gesture_recognizer(tap_recognizer)
    call view%add_gesture_recognizer(swipe_recognizer)

contains

    subroutine tap_handler(gesture)
        type(touch_gesture), intent(in) :: gesture
        location = gesture%location_in_view(view)
        print *, "Tap at:", location%x, location%y
    end subroutine tap_handler

    subroutine swipe_handler(gesture)
        type(touch_gesture), intent(in) :: gesture
        print *, "Swipe detected"
        call navigate_to_next_screen()
    end subroutine swipe_handler

end program touch_app
```

### Mobile Navigation

```fortran
! Mobile navigation patterns
program mobile_navigation
    use forge_mobile_navigation

    type(navigation_controller) :: nav_controller
    type(view_controller) :: home_vc, detail_vc, settings_vc

    ! Set up navigation stack
    call nav_controller%push_view_controller(home_vc, .true.)

    ! Handle navigation
    call home_vc%set_navigation_callback(home_navigation_handler)

contains

    subroutine home_navigation_handler(action)
        character(len=*), intent(in) :: action

        select case (action)
        case ("show_detail")
            call nav_controller%push_view_controller(detail_vc, .true.)
        case ("show_settings")
            call nav_controller%push_view_controller(settings_vc, .true.)
        end select
    end subroutine home_navigation_handler

end program mobile_navigation
```

## Mobile-Specific Widgets

### Mobile-Optimized Controls

```fortran
! Mobile-specific widgets
program mobile_widgets
    use forge_mobile_widgets

    type(date_picker) :: date_picker
    type(time_picker) :: time_picker
    type(slider) :: slider
    type(switch) :: toggle_switch

    ! Date picker (native mobile control)
    call date_picker%set_date_mode(DATE_MODE_DATE)
    call date_picker%set_callback(date_selected_handler)

    ! Time picker
    call time_picker%set_time_mode(TIME_MODE_HOUR_MINUTE)
    call time_picker%set_callback(time_selected_handler)

    ! Mobile slider
    call slider%set_range(0.0, 100.0)
    call slider%set_value(50.0)
    call slider%set_callback(value_changed_handler)

    ! Toggle switch
    call toggle_switch%set_on(.true.)
    call toggle_switch%set_callback(toggle_changed_handler)

contains

    subroutine date_selected_handler(date)
        type(date), intent(in) :: date
        print *, "Selected date:", date%year, date%month, date%day
    end subroutine date_selected_handler

    subroutine time_selected_handler(time)
        type(time), intent(in) :: time
        print *, "Selected time:", time%hour, time%minute
    end subroutine time_selected_handler

    subroutine value_changed_handler(value)
        real, intent(in) :: value
        print *, "Slider value:", value
    end subroutine value_changed_handler

    subroutine toggle_changed_handler(is_on)
        logical, intent(in) :: is_on
        if (is_on) then
            print *, "Switch turned ON"
        else
            print *, "Switch turned OFF"
        end if
    end subroutine toggle_changed_handler

end program mobile_widgets
```

### Pull-to-Refresh

```fortran
! Pull-to-refresh functionality
program pull_to_refresh_app
    use forge_mobile_pull_to_refresh

    type(pull_to_refresh_view) :: refresh_view
    type(scroll_view) :: scroll_view

    ! Set up pull-to-refresh
    call refresh_view%init()
    call refresh_view%set_refresh_callback(refresh_handler)
    call scroll_view%add_pull_to_refresh(refresh_view)

contains

    subroutine refresh_handler()
        ! Simulate network request
        call perform_data_refresh()

        ! End refreshing after data loads
        call refresh_view%end_refreshing()
    end subroutine refresh_handler

    subroutine perform_data_refresh()
        ! Refresh data from server
        call load_latest_data()
        call update_ui_with_new_data()
    end subroutine perform_data_refresh

end program pull_to_refresh_app
```

## Mobile Development Best Practices

### Platform Conventions

#### iOS Patterns

```fortran
! iOS-specific patterns
program ios_app
    use forge_ios_patterns

    ! Use iOS navigation patterns
    call setup_ios_navigation_bar()
    call setup_ios_tab_bar()

    ! iOS-specific UI elements
    call add_ios_segmented_control()
    call add_ios_table_view()

    ! iOS gesture handling
    call setup_ios_gesture_recognizers()

end program ios_app
```

#### Android Patterns

```fortran
! Android-specific patterns
program android_app
    use forge_android_patterns

    ! Use Android navigation patterns
    call setup_android_navigation_drawer()
    call setup_android_bottom_navigation()

    ! Android-specific UI elements
    call add_android_floating_action_button()
    call add_android_recycler_view()

    ! Android gesture handling
    call setup_android_gesture_detectors()

end program android_app
```

### Mobile Performance

```fortran
! Mobile performance optimizations
program mobile_performance
    use forge_mobile_performance

    ! Lazy loading
    call enable_lazy_loading_for_list()

    ! Image optimization
    call optimize_images_for_mobile()

    ! Memory management
    call implement_memory_warning_handler()

    ! Battery optimization
    call reduce_background_activity()

end program mobile_performance
```

### Mobile Testing

```fortran
! Mobile testing patterns
program mobile_testing
    use forge_mobile_testing

    ! Device orientation testing
    call test_portrait_orientation()
    call test_landscape_orientation()

    ! Touch interaction testing
    call test_touch_gestures()
    call test_multi_touch()

    ! Network condition testing
    call test_offline_mode()
    call test_slow_network()

end program mobile_testing
```

## Mobile Deployment

### iOS App Store

#### iOS Build Configuration

```ruby
# iOS build configuration (for RubyMotion or similar)
# This would be handled by the ForGE iOS backend

app.name = 'My ForGE App'
app.version = '1.0.0'
app.deployment_target = '12.0'
app.device_family = [:iphone, :ipad]
app.interface_orientations = [:portrait, :landscape_left, :landscape_right]

# ForGE-specific configuration
app.vendor_project('vendor/forge-ios', :static)
app.frameworks += ['UIKit', 'Foundation', 'CoreGraphics']
```

#### iOS Info.plist

```xml
<!-- iOS Info.plist -->
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleExecutable</key>
    <string>MyForGEApp</string>
    <key>CFBundleIdentifier</key>
    <string>com.yourcompany.myforgeapp</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>My ForGE App</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0</string>
    <key>CFBundleVersion</key>
    <string>1</string>
    <key>LSRequiresIPhoneOS</key>
    <true/>
    <key>UIRequiredDeviceCapabilities</key>
    <array>
        <string>armv7</string>
    </array>
    <key>UISupportedInterfaceOrientations</key>
    <array>
        <string>UIInterfaceOrientationPortrait</string>
        <string>UIInterfaceOrientationLandscapeLeft</string>
        <string>UIInterfaceOrientationLandscapeRight</string>
    </array>
</dict>
</plist>
```

### Android Deployment

#### Android Build Configuration

```gradle
// Android build.gradle
plugins {
    id 'com.android.application'
}

android {
    compileSdk 33

    defaultConfig {
        applicationId "com.yourcompany.myforgeapp"
        minSdk 21
        targetSdk 33
        versionCode 1
        versionName "1.0.0"
    }

    buildTypes {
        release {
            minifyEnabled false
            proguardFiles getDefaultProguardFile('proguard-android-optimize.txt'), 'proguard-rules.pro'
        }
    }

    // ForGE-specific configuration
    externalNativeBuild {
        cmake {
            path "src/main/cpp/CMakeLists.txt"
        }
    }
}
```

#### Android Manifest

```xml
<!-- Android AndroidManifest.xml -->
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.yourcompany.myforgeapp">

    <uses-permission android:name="android.permission.INTERNET" />
    <uses-permission android:name="android.permission.ACCESS_NETWORK_STATE" />

    <application
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:theme="@style/AppTheme">

        <activity
            android:name=".MainActivity"
            android:exported="true">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>

    </application>

</manifest>
```

### Cross-Platform Mobile Development

```fortran
! Cross-platform mobile code
program cross_platform_mobile
    use forge_mobile

    ! Platform detection
    if (is_ios()) then
        call setup_ios_specific_features()
    else if (is_android()) then
        call setup_android_specific_features()
    end if

    ! Shared mobile features
    call setup_common_mobile_features()

contains

    subroutine setup_ios_specific_features()
        ! iOS-specific setup
        call setup_ios_push_notifications()
        call setup_ios_in_app_purchases()
    end subroutine setup_ios_specific_features

    subroutine setup_android_specific_features()
        ! Android-specific setup
        call setup_android_push_notifications()
        call setup_android_in_app_billing()
    end subroutine setup_android_specific_features

    subroutine setup_common_mobile_features()
        ! Cross-platform features
        call setup_touch_gestures()
        call setup_mobile_layout()
        call setup_data_synchronization()
    end subroutine setup_common_mobile_features

end program cross_platform_mobile
```

## Mobile-Specific Challenges

### Screen Size Variations

```fortran
! Handling different screen sizes
program responsive_design
    use forge_mobile_responsive

    type(device_info) :: device

    device = get_device_info()

    ! Adjust UI based on screen size
    if (device%screen_size < 5.0) then
        ! Small phone
        call use_compact_layout()
    else if (device%screen_size < 7.0) then
        ! Large phone/small tablet
        call use_standard_layout()
    else
        ! Large tablet
        call use_expanded_layout()
    end if

    ! Handle different pixel densities
    if (device%pixel_density > 2.0) then
        call use_high_res_assets()
    else
        call use_standard_assets()
    end if

end program responsive_design
```

### Touch vs Mouse Input

```fortran
! Handling different input methods
program input_handling
    use forge_input

    type(input_device) :: device

    device = get_primary_input_device()

    if (device%type == INPUT_TOUCH) then
        ! Touch device (phone/tablet)
        call setup_touch_optimizations()
        call increase_button_sizes()
        call add_touch_feedback()
    else if (device%type == INPUT_MOUSE) then
        ! Mouse device (desktop/laptop with mouse)
        call setup_mouse_optimizations()
        call use_precise_hit_targets()
        call add_hover_effects()
    end if

end program input_handling
```

### Mobile Network Considerations

```fortran
! Mobile network awareness
program network_aware_app
    use forge_mobile_network

    type(network_info) :: network

    network = get_network_info()

    if (network%type == NETWORK_CELLULAR) then
        ! Cellular connection - be conservative
        call reduce_image_quality()
        call disable_auto_refresh()
        call show_data_usage_warning()
    else if (network%type == NETWORK_WIFI) then
        ! WiFi connection - use full features
        call use_high_quality_assets()
        call enable_auto_refresh()
    end if

    ! Handle network changes
    call set_network_change_callback(network_changed_handler)

contains

    subroutine network_changed_handler(new_network)
        type(network_info), intent(in) :: new_network

        if (new_network%available) then
            call resume_data_operations()
        else
            call pause_data_operations()
            call show_offline_message()
        end if
    end subroutine network_changed_handler

end program network_aware_app
```

### Battery Optimization

```fortran
! Battery-aware programming
program battery_optimized_app
    use forge_mobile_battery

    type(battery_info) :: battery

    battery = get_battery_info()

    if (battery%level < 20) then
        ! Low battery - reduce activity
        call disable_animations()
        call reduce_update_frequency()
        call show_battery_warning()
    end if

    if (.not. battery%is_charging) then
        ! On battery - optimize for power
        call use_dark_theme()
        call disable_background_location()
        call reduce_gps_accuracy()
    end if

end program battery_optimized_app
```

## Next Steps

- Read the [desktop guides](desktop_guide.md) for traditional GUI development
- Learn about [performance optimization](performance_guide.md) for mobile
- Explore [deployment strategies](deployment_guide.md) for app stores
- Study [testing mobile apps](testing_guide.md)