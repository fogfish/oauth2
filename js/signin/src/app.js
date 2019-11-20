import React from 'react'
import {
  HashRouter as Router,
  Route
} from 'react-router-dom'
import SignIn from './components/SignIn'
import SignUp from './components/SignUp'
import { SecretReset } from 'components/SecretReset'
import { SecretRecover } from 'components/SecretRecover'
import { ThemeProvider } from 'emotion-theming'
import queryString from 'query-string'
import './app.css'

const { response_type, client_id, state, ...params } = queryString.parse(window.location.search)
const oauth2 = { 
  ...params, 
  response_type: response_type || "code", 
  client_id: client_id || "account@oauth2", 
  state: state || ''
}

const theme = {
  breakpoints: ['480px', '768px', '1024px'],
  space: [0, 4, 8, 16, 32, 64, 128, 256, 512],
}

const App = (props) => (
  <ThemeProvider theme={theme}>
    <Router hashType="noslash">
      <>
        <Route exact path="/" component={() => <SignIn { ...oauth2 } />} />
        <Route exact path="/signup" component={() => <SignUp { ...oauth2} />} />
        <Route exact path="/reset" component={() => <SecretReset { ...oauth2 } />} />
        <Route exact path="/recover" component={() => <SecretRecover { ...oauth2 } />} />
      </>
    </Router>
  </ThemeProvider>
)

export default App
