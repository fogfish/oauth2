//
//
import { createStore, combineReducers } from 'redux'

//
//
import * as core from './ducks/core'

const root = combineReducers({
   core: core.default
})

export const store  = createStore(root)
