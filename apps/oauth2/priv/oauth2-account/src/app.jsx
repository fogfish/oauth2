import React from 'react';
import './app.css';

import {Container} from './components/dress-code'

import {Account} from './components/account'
import {Register, Credentials} from './components/oauth-app'

const App = () => (
   <Container>
      <Account />
      <Register />
      <Credentials />
   </Container>
);


export default App;