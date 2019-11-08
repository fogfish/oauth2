import React from 'react'
import {
  BrowserRouter as Router,
  Route
} from 'react-router-dom'
import { Account } from './components/Account'
import { NewApp } from './components/NewApp'
import './app.css'

const App = () => (
  <div>
    <Router>
      <div>
        <Route exact path="/oauth2/account" component={ Account } />
        <Route exact path="/oauth2/account/app" component={ NewApp } />
      </div>
    </Router>
  </div>
)

export default App
