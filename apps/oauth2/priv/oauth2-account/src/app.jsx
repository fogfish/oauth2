import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import './app.css';

import {Container} from './components/dress-code'
import Account from './components/account'
import OAuthAppRegister from './components/oauth-app-register'
import OAuthAppSecret from './components/oauth-app-secret'

const App = ({core}) => (
   <Container>
      {core.isOAuthApps && <Account />}
      {core.isOAuthAppRegister && <OAuthAppRegister />}
      {core.isOAuthAppCredentials && <OAuthAppSecret />}
   </Container>
);

//
// Visual Map 
const vm = (state) => (state)
const dm = (dispatch) => ({actions: bindActionCreators({}, dispatch)})

export default connect(vm, dm)(App)
