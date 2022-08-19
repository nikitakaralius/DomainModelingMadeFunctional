namespace OrderTaking.Domain

open System
open System.Text.RegularExpressions

/// Constrained to be 50 chars or less, not null
type String50 = private String50 of string

/// An email address
type EmailAddress = private EmailAddress of string

// A zip code
type ZipCode = private ZipCode of string

/// The codes for "Widgets". Starting with "W" then 4 digits
type WidgetCode = private WidgetCode of string

/// The codes for "Gizmos". Starting with "G" then 3 digits
type GizmoCode = private GizmoCode of string

/// Constrained to be an integer between 1 and 1000
type UnitQuantity = private UnitQuantity of int

/// Constrained to be a decimal between 0.05 and 100.0
type KilogramQuantity = private KilogramQuantity of decimal

/// An Id for orders. Constrained to be a non-empty string < 10 chars
type OrderId = private OrderId of string

/// An Id for order lines. Constrained to be a non-empty string < 10 chars.
type OrderLineId = private OrderLineId of string

/// Constrained to be > 0.0
type Price = private Price of decimal

/// Constrained to be > 0.0
type BillingAmount = private BillingAmount of decimal


type OrderQuantity =
    | Unit of UnitQuantity
    | Kilos of KilogramQuantity

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode


module ConstrainedType =

    let createString fieldName ctor maxLen str =
        if str |> String.IsNullOrEmpty then
            let msg =
                sprintf "%s must not be null or empty" fieldName

            Error msg
        elif str.Length > maxLen then
            let msg =
                sprintf "%s must not be more than %i chars" fieldName maxLen

            Error msg
        else
            Ok(ctor str)


    let createStringOption fieldName ctor maxLen str =
        if str |> String.IsNullOrEmpty then
            Ok None
        elif str.Length > maxLen then
            let msg =
                sprintf "%s must not be more than %i chars" fieldName maxLen

            Error msg
        else
            Ok(ctor str |> Some)


    let createInt fieldName ctor minVal maxVal x =
        if x < minVal then
            let msg =
                sprintf "%s must not be less than %i" fieldName minVal

            Error msg
        elif x > maxVal then
            let msg =
                sprintf "%s must not be greater than %i" fieldName maxVal

            Error msg
        else
            Ok(ctor x)

    let createLike fieldName ctor pattern str =
        if str |> String.IsNullOrEmpty then
            let msg =
                sprintf "%s must not be null or empty" fieldName

            Error msg
        elif Regex.IsMatch(str, pattern) then
            Ok(ctor str)
        else
            let msg =
                sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern

            Error msg

    let createDecimal fieldName ctor minVal maxVal x =
        if x < minVal then
            let msg =
                sprintf "%s must not be less than %M" fieldName minVal

            Error msg
        elif x > maxVal then
            let msg =
                sprintf "%s must not be greater than %M" fieldName maxVal

            Error msg
        else
            Ok(ctor x)


module String50 =

    let value (String50 s) = s

    let create fieldName x =
        ConstrainedType.createString fieldName String50 50 x

    let createOption fieldName x =
        ConstrainedType.createStringOption fieldName String50 50 x

module UnitQuantity =

    let value (UnitQuantity qty) = qty

    let create fieldName x =
        ConstrainedType.createInt fieldName UnitQuantity 1 1000 x
