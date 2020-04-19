from aiohttp_graphql import GraphQLView
from graphql.execution.executors.asyncio import AsyncioExecutor

from .schema import schema



gql_view = GraphQLView(schema=schema, graphiql=True, executor=AsyncioExecutor())