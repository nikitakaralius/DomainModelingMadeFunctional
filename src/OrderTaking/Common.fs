namespace OrderTaking.Common

open System
open System.Text.RegularExpressions

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
