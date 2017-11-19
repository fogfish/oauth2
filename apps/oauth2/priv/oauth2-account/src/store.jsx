//
//
import { createStore, applyMiddleware, combineReducers } from 'redux'
import { createLogger } from 'redux-logger'
import thunk from 'redux-thunk'

//
//
import * as access_token from './ducks/access-token'
import * as core from './ducks/core'
import * as app from './ducks/oauth-app'
import * as account from './ducks/account'

const root = combineReducers({
   access_token: access_token.default,
   core: core.default,
   app: app.default,
   account: account.default
})

const logger = createLogger()

export const store  = createStore(root, applyMiddleware(thunk, logger))

store.dispatch(access_token.signin())
