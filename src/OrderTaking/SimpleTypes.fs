namespace OrderTaking.Domain

open System
open OrderTaking.Common

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

/// An Id for orders. Constrained to be a non-empty string < 50 chars
type OrderId = private OrderId of string

/// An Id for order lines. Constrained to be a non-empty string < 50 chars.
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


module String50 =

    let value (String50 s) = s

    let create fieldName x =
        ConstrainedType.createString fieldName String50 50 x

    let createOption fieldName x =
        ConstrainedType.createStringOption fieldName String50 50 x


module EmailAddress =

    let value (EmailAddress address) = address

    let create fieldName x =
        let pattern = ".+@.+"
        ConstrainedType.createLike fieldName EmailAddress pattern x


module ZipCode =

    let value (ZipCode zc) = zc

    let create fieldName x =
        let pattern = "\d{5,10}"
        ConstrainedType.createLike fieldName ZipCode pattern x


module WidgetCode =

    let value (WidgetCode c) = c

    let create fieldName x =
        let pattern = "W\d{4}"
        ConstrainedType.createLike fieldName WidgetCode pattern x


module GizmoCode =

    let value (GizmoCode c) = c

    let create fieldName x =
        let pattern = "G\d{3}"
        ConstrainedType.createLike fieldName GizmoCode pattern x


module UnitQuantity =

    let value (UnitQuantity q) = q

    let create fieldName x =
        ConstrainedType.createInt fieldName UnitQuantity 1 1000 x


module KilogramQuantity =

    let value (KilogramQuantity q) = q

    let create fieldName x =
        ConstrainedType.createDecimal fieldName KilogramQuantity 0.05m 1000.0m x


module OrderId =

    let value (OrderId id) = id

    let create fieldName x =
        ConstrainedType.createString fieldName OrderId 50 x


module OrderLineId =

    let value (OrderLineId id) = id

    let create fieldName x =
        ConstrainedType.createString fieldName OrderLineId 50 x


module Price =

    let value (Price p) = p

    let create fieldName x =
        ConstrainedType.createDecimal fieldName Price 0.0m Decimal.MaxValue x


module BillingAmount =

    let value (BillingAmount b) = b

    let create fieldName x =
        ConstrainedType.createDecimal fieldName BillingAmount 0.0m Decimal.MaxValue x

    let sum prices = prices |> List.sumBy Price.value


module OrderQuantity =

    let value qty =
        match qty with
        | Unit (UnitQuantity u) -> u |> decimal
        | Kilos (KilogramQuantity k) -> k

    let create fieldName productCode amount =
        match productCode with
        | Widget _ ->
            UnitQuantity.create fieldName (int amount)
            |> Result.map OrderQuantity.Unit
        | Gizmo _ ->
            KilogramQuantity.create fieldName amount
            |> Result.map OrderQuantity.Kilos


module ProductCode =

    let value code =
        match code with
        | Widget (WidgetCode wc) -> wc
        | Gizmo (GizmoCode gc) -> gc

    let create fieldName code =
        if code |> String.IsNullOrEmpty then
            let msg =
                sprintf "%s must not be null or empty" code

            Error msg
        else
            match code[0] with
            | 'W' ->
                WidgetCode.create fieldName code
                |> Result.map ProductCode.Widget
            | 'G' ->
                GizmoCode.create fieldName code
                |> Result.map ProductCode.Gizmo
            | _ ->
                let msg =
                    sprintf "%s: format not recognized" code

                Error msg
