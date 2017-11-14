import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import './app.css';
import {Container, SignIn, SignUp} from './components/oauth2.jsx'

import {signin, signup} from './ducks/core'

const OAuth2 = ({core, actions}) => (
  <Container>
    {core.isSignIn && <SignIn onSignUp={actions.signup}/>}
    {core.isSignUp && <SignUp onSignIn={actions.signin}/>}
  </Container>
)

//
// Visual Map 
const vm = (state) => (state)
const dm = (dispatch) => ({actions: bindActionCreators({signin, signup}, dispatch)})

export default connect(vm, dm)(OAuth2)
