// Note: shall be first line to capture OAuth2 token 
import store from './store'

import React from 'react'
import { Provider } from 'react-redux'
import ReactDOM from 'react-dom'
import App from './App'


ReactDOM.render(
  <Provider store={store}>
    <App />
  </Provider>,
  document.getElementById('root')
)
