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
import { default as notification } from './components/Notification/ducks'

const root = combineReducers({
   oauth2,
   app,
   registry,
   notification
})

// const logger = createLogger()
// const store  = createStore(root, applyMiddleware(thunk, logger))
const store  = createStore(root, applyMiddleware(thunk))
store.dispatch(accessToken())

export default store
