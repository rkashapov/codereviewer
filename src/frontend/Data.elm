module Data exposing (..)

import Api.Mutation
import Api.Object
import Api.Object.Change as ChangeType
import Api.Object.Comment as CommentType
import Api.Object.Commit as CommitType
import Api.Object.Diff as DiffType
import Api.Object.GitCommit as GitCommitType
import Api.Object.GitCommitPerson as GitCommitPersonType
import Api.Object.Organization as OrganizationType
import Api.Object.Repository as RepositoryType
import Api.Object.User as UserType
import Api.Query as Query
import Api.Scalar
import Api.ScalarCodecs
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.SelectionSet exposing (SelectionSet, nonNullElementsOrFail, with)
import RemoteData exposing (RemoteData)


type Msg
    = ViewerLoaded (RemoteData (Graphql.Http.Error Viewer) Viewer)
    | UpsertCommentDone (RemoteData (Graphql.Http.Error Comment) Comment)


type alias Viewer =
    { id : Api.ScalarCodecs.Id
    , name : Maybe String
    , email : Maybe String
    , login : Maybe String
    , avatarUrl : Maybe String
    , organization : Organization
    }


type alias Organization =
    { name : String
    , repository : Repository
    }


type alias Repository =
    { name : String
    , compare : Diff
    }


type alias Diff =
    { url : String
    , files : List Change
    , comments : List Comment
    , commits : List Commit
    }


type alias Comment =
    { author : GitHubUser
    , sha : String
    , lineno : Int
    , text : String
    }


type alias GitHubUser =
    { id : Api.ScalarCodecs.Id
    , name : Maybe String
    , email : Maybe String
    , login : Maybe String
    , avatarUrl : Maybe String
    }


type alias Change =
    { sha : String
    , filename : String
    , additions : Int
    , deletions : Int
    , changes : Int
    , blobUrl : String
    , rawUrl : String
    , contentsUrl : String
    , patch : String
    }


type alias CompareArgs =
    { owner : String
    , repository : String
    , base : String
    , head : String
    }


type alias Commit =
    { url : String
    , htmlUrl : String
    , commit : GitCommit
    , author : GitHubUser
    }


type alias GitCommit =
    { url : String
    , author : GitCommitPerson
    , committer : GitCommitPerson
    , message : String
    }


type alias GitCommitPerson =
    { name : String
    , email : String
    }


query : CompareArgs -> SelectionSet Viewer RootQuery
query args =
    Query.viewer (userInfoSelection args)


userInfoSelection : CompareArgs -> SelectionSet Viewer Api.Object.User
userInfoSelection args =
    Graphql.SelectionSet.map6 Viewer
        UserType.id
        UserType.name
        UserType.email
        UserType.login
        UserType.avatarUrl
        (UserType.organization { name = args.owner } (organizationSelection args))


organizationSelection : CompareArgs -> SelectionSet Organization Api.Object.Organization
organizationSelection args =
    Graphql.SelectionSet.map2 Organization
        OrganizationType.name
        (OrganizationType.repository { name = args.repository } (repositorySelection args))


repositorySelection : CompareArgs -> SelectionSet Repository Api.Object.Repository
repositorySelection args =
    Graphql.SelectionSet.map2 Repository
        RepositoryType.name
        (RepositoryType.compare
            { base = args.base, head = args.head }
            compareSelection
        )


compareSelection : SelectionSet Diff Api.Object.Diff
compareSelection =
    Graphql.SelectionSet.succeed Diff
        |> with DiffType.url
        |> with (nonNullElementsOrFail (DiffType.files changeSelection))
        |> with (nonNullElementsOrFail (DiffType.comments commentsSelection))
        |> with (nonNullElementsOrFail (DiffType.commits commitsSelection))


changeSelection : SelectionSet Change Api.Object.Change
changeSelection =
    Graphql.SelectionSet.succeed Change
        |> with ChangeType.sha
        |> with ChangeType.filename
        |> with ChangeType.additions
        |> with ChangeType.deletions
        |> with ChangeType.changes
        |> with ChangeType.blobUrl
        |> with ChangeType.rawUrl
        |> with ChangeType.contentsUrl
        |> with ChangeType.patch


commentsSelection : SelectionSet Comment Api.Object.Comment
commentsSelection =
    Graphql.SelectionSet.succeed Comment
        |> with (CommentType.author githubUserSelection)
        |> with CommentType.sha
        |> with CommentType.lineno
        |> with CommentType.text


commitsSelection : SelectionSet Commit Api.Object.Commit
commitsSelection =
    Graphql.SelectionSet.succeed Commit
        |> with CommitType.url
        |> with CommitType.htmlUrl
        |> with (CommitType.commit gitCommitSelection)
        |> with (CommitType.author githubUserSelection)


gitCommitSelection : SelectionSet GitCommit Api.Object.GitCommit
gitCommitSelection =
    let
        gitCommitPersonSelection =
            Graphql.SelectionSet.succeed GitCommitPerson
                |> with GitCommitPersonType.name
                |> with GitCommitPersonType.email
    in
    Graphql.SelectionSet.succeed GitCommit
        |> with GitCommitType.url
        |> with (GitCommitType.author gitCommitPersonSelection)
        |> with (GitCommitType.committer gitCommitPersonSelection)
        |> with GitCommitType.message


githubUserSelection : SelectionSet GitHubUser Api.Object.User
githubUserSelection =
    Graphql.SelectionSet.map5 GitHubUser
        UserType.id
        UserType.name
        UserType.email
        UserType.login
        UserType.avatarUrl


makeRequest : CompareArgs -> String -> Cmd Msg
makeRequest args apiUrl =
    query args
        |> Graphql.Http.queryRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> ViewerLoaded)


upsertComment : String -> Int -> String -> SelectionSet Comment RootMutation
upsertComment sha lineno text =
    Api.Mutation.upsertComment
        { sha = sha
        , lineno = lineno
        , text = text
        }
        commentsSelection
        |> Graphql.SelectionSet.map
            (Maybe.withDefault
                { author =
                    { id = Api.Scalar.Id ""
                    , name = Nothing
                    , login = Nothing
                    , email = Nothing
                    , avatarUrl = Nothing
                    }
                , sha = ""
                , lineno = 0
                , text = ""
                }
            )


makeUpsertCommentRequest : String -> Int -> String -> String -> Cmd Msg
makeUpsertCommentRequest sha lineno text apiUrl =
    upsertComment sha lineno text
        |> Graphql.Http.mutationRequest apiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> UpsertCommentDone)
