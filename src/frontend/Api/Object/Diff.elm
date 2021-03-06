-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Diff exposing (..)

import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.ScalarCodecs
import Api.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


url : SelectionSet String Api.Object.Diff
url =
    Object.selectionForField "String" "url" [] Decode.string


files : SelectionSet decodesTo Api.Object.Change -> SelectionSet (List (Maybe decodesTo)) Api.Object.Diff
files object_ =
    Object.selectionForCompositeField "files" [] object_ (identity >> Decode.nullable >> Decode.list)


comments : SelectionSet decodesTo Api.Object.Comment -> SelectionSet (List (Maybe decodesTo)) Api.Object.Diff
comments object_ =
    Object.selectionForCompositeField "comments" [] object_ (identity >> Decode.nullable >> Decode.list)


commits : SelectionSet decodesTo Api.Object.Commit -> SelectionSet (List (Maybe decodesTo)) Api.Object.Diff
commits object_ =
    Object.selectionForCompositeField "commits" [] object_ (identity >> Decode.nullable >> Decode.list)
