import React from 'react'
import './App.css'

import {
  HashRouter as Router,
  Route
} from 'react-router-dom'

import { Page, Container } from 'react-dress-code'
import SignIn from './components/SignIn'
import SignUp from './components/SignUp'
import { SecretReset } from 'components/SecretReset'
import { SecretRecover } from 'components/SecretRecover'
import queryString from 'query-string'

const { response_type, client_id, state, ...params } = queryString.parse(window.location.search)
const oauth2 = { 
  ...params, 
  response_type: response_type || "code", 
  client_id: client_id || "oauth2-account", 
  state: state || ''
}

const App = (props) => (
  <Page>
    <Container limited>
      <Router hashType="noslash">
        <React.Fragment>
          <Route exact path="/" component={() => <SignIn { ...oauth2 } />} />
          <Route exact path="/signup" component={() => <SignUp { ...oauth2} />} />
          <Route exact path="/reset" component={() => <SecretReset { ...oauth2 } />} />
          <Route exact path="/recover" component={() => <SecretRecover { ...oauth2 } />} />
        </React.Fragment>
      </Router>
    </Container>
  </Page>
)

export default App
