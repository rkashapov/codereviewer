-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Organization exposing (..)

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


name : SelectionSet String Api.Object.Organization
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias RepositoryRequiredArguments =
    { name : String }


repository : RepositoryRequiredArguments -> SelectionSet decodesTo Api.Object.Repository -> SelectionSet decodesTo Api.Object.Organization
repository requiredArgs object_ =
    Object.selectionForCompositeField "repository" [ Argument.required "name" requiredArgs.name Encode.string ] object_ identity
