//
//
import { createStore, applyMiddleware, combineReducers } from 'redux'
import { createLogger } from 'redux-logger'

//
//
import * as core from './ducks/core'

const root = combineReducers({
   core: core.default
})

const logger = createLogger()

export const store  = createStore(root, applyMiddleware(logger))
