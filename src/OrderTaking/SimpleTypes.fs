namespace OrderTaking.Domain

open OrderTaking.Common
open System
open System.Text.RegularExpressions

/// Constrained to be 50 chars or less, not null
type String50 = private String50 of string

type EmailAddress = private EmailAddress of string

/// Starting with "W" then 4 digits
type WidgetCode = private WidgetCode of string

/// Starting with "G" then 3 digits
type GizmoCode = private GizmoCode of string

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

/// Constrained to be an integer between 1 and 1000
type UnitQuantity = private UnitQuantity of int

/// Constrained to be a decimal between 0.05 and 100.0
type KilogramQuantity = KilogramQuantity of decimal

type OrderQuantity =
    | Unit of UnitQuantity
    | Kilos of KilogramQuantity

type OrderId = Undefined
type OrderLineId = Undefined
type CustomerId = Undefined

type CustomerInfo = Undefined
type ShippingAddress = Undefined
type BillingAddress = Undefined
type Price = Undefined
type BillingAmount = Undefined

type Order = {
    Id: OrderId
    CustomerId: CustomerId
    ShippingAddress: ShippingAddress
    BillingAddress: BillingAddress
    OrderLines: OrderLine list
    AmountToBill: BillingAmount
}

and OrderLine = {
    Id: OrderLineId
    OrderId: OrderId
    ProductCode: ProductCode
    OrderQuantity: OrderQuantity
    Price: Price
}

type UnvalidatedOrder = {
    OrderId: string
    CustomerInfo: ...
    ShippingAddress: ...
}

type PlacedOrderEvents = {
    AcknowledgmentSent: ...
    OrderPlaced: ...
    BillableOrderPlaced: ...
}

type PlaceOrderError =
    | ValidationError of ValidationError list

and ValidationError = {
    FieldName: string
    ErrorDescription: string
}

type PlaceOrder = UnvalidatedOrder -> Result<PlacedOrderEvents, PlaceOrderError>


module ConstrainedType =

    let createString fieldName ctor maxLen str =
        if str |> String.IsNullOrEmpty then
            let msg = sprintf "%s must not be null or empty" fieldName
            Error msg
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen
            Error msg
        else
            Ok (ctor str)


    let createStringOption fieldName ctor maxLen str =
        if str |> String.IsNullOrEmpty then
            Ok None
        elif str.Length > maxLen then
            let msg = sprintf "%s must not be more than %i chars" fieldName maxLen
            Error msg
        else
            Ok (ctor str |> Some)


    let createInt fieldName ctor minVal maxVal x =
        if x < minVal then
            let msg = sprintf "%s must not be less than %i" fieldName minVal
            Error msg
        elif x > maxVal then
            let msg = sprintf "%s must not be greater than %i" fieldName maxVal
            Error msg
        else
            Ok (ctor x)

    let createLike fieldName ctor pattern str =
       if str |> String.IsNullOrEmpty then
           let msg = sprintf "%s must not be null or empty" fieldName
           Error msg
       elif Regex.IsMatch(str, pattern) then
           Ok (ctor str)
       else
           let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
           Error msg

    let createDecimal fieldName ctor minVal maxVal x =
        if x < minVal then
            let msg = sprintf "%s must not be less than %M" fieldName minVal
            Error msg
        elif x > maxVal then
            let msg = sprintf "%s must not be greater than %M" fieldName maxVal
            Error msg
        else
            Ok (ctor x)


module UnitQuantity =

    let value (UnitQuantity qty) = qty

    let create fieldName x =
        ConstrainedType.createInt fieldName UnitQuantity 1 1000 x
