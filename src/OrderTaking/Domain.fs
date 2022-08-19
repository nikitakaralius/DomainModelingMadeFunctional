namespace OrderTaking.Domain

open OrderTaking.Common

// constraint: starting with "W" then 4 digits
type WidgetCode = WidgetCode of string

// constraint: starting with "G" then 3 digits
type GizmoCode = GizmoCode of string

type ProductCode =
    | Widget of WidgetCode
    | Gizmo of GizmoCode

type UnitQuantity = UnitQuantity of int

// constraint: range(0.05, 1000.0)
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
