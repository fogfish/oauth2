import React from 'react'
import {
  BrowserRouter as Router,
  Route
} from 'react-router-dom'
import { OAuth2 } from './components/OAuth2'
import { Account } from './components/Account'
import { NewApp } from './components/NewApp'
import { Notification } from './components/Notification'
import './app.css'

const App = () => (
  <div>
    {/* <Notification /> */}
    <OAuth2>
      <Router>
        <div>
          <Route exact path="/oauth2/account" component={ Account } />
          <Route exact path="/oauth2/account/app" component={ NewApp } />
        </div>
      </Router>
    </OAuth2>
  </div>
)

export default App
