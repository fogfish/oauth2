// Note: shall be first line to capture OAuth2 token 
import store from './store'

import React from 'react'
import ReactDOM from 'react-dom'
import {
  BrowserRouter as Router,
  Route
} from 'react-router-dom'
import { Provider } from 'react-redux'
import { OAuth2 } from './components/OAuth2'
import { Account } from './components/Account'
import { NewApp } from './components/NewApp'
import './app.css'


ReactDOM.render(
  <Provider store={store}>
    <OAuth2>
      <Router>
        <div>
          <Route exact path="/oauth2/account" component={ Account } />
          <Route exact path="/oauth2/account/app" component={ NewApp } />
        </div>
      </Router>
    </OAuth2>
  </Provider>,
  document.getElementById('root')
)
