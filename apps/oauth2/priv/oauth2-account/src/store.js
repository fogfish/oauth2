//
//
import { createStore, applyMiddleware, combineReducers } from 'redux'
import { createLogger } from 'redux-logger'
import thunk from 'redux-thunk'

//
//
import { default as oauth2, accessToken } from './components/OAuth2/ducks'
import { default as app } from './components/NewApp/ducks'
import { default as registry } from './components/Registry/ducks'

const root = combineReducers({
   oauth2,
   app,
   registry
})

const logger = createLogger()
const store  = createStore(root, applyMiddleware(thunk, logger))
store.dispatch(accessToken())

export default store
