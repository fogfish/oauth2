import React from 'react'
import { withStateHandlers } from 'recompose'

import { Container } from 'react-dress-code'
import SignIn from './components/SignIn'
import SignUp from './components/SignUp'
import './app.css';

const OAuth2 = (props) => (
  <Container>
    {props.isSignIn ? <SignIn { ...props } /> : <SignUp { ...props } />}
  </Container>
)

const onSignUp = ({ isSignIn }) => () => ({ isSignIn: false})
const onSignIn = ({ isSignIn }) => () => ({ isSignIn: true})

const App = withStateHandlers(
  { isSignIn: true },
  { onSignUp, onSignIn }
)(OAuth2)

export default App
