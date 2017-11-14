//
//
import { createStore, applyMiddleware, combineReducers } from 'redux'
import { createLogger } from 'redux-logger'
// import thunk from 'redux-thunk'
// import initSubscriber from 'redux-subscriber'

//
//
import * as core from './ducks/core'

const root = combineReducers({
   core: core.default
})

const logger = createLogger()

export const store  = createStore(root, applyMiddleware(logger))
// export const subscribe = initSubscriber(store)
