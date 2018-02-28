import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import './app.css';

import {Container, MessageFailure, LoadingBar, Window, Dialog} from './components/dress-code'
import Account from './components/account'
import OAuthAppRegister from './components/oauth-app-register'
import OAuthAppSecret from './components/oauth-app-secret'
import {signout} from './ducks/access-token';

import { Tab, TabElement } from 'react-dress-code'


const Failure = ({error, onClick}) => (
   <Window>
      <Dialog title="Error" subtitle="with authorization">
         <p>
            {error}
         </p>
      </Dialog>
      <div className="dc-dialog__actions">
         <button className="dc-btn dc-btn--primary" onClick={onClick}>Sign In</button>
      </div>
   </Window>
)

const Core = ({core}) => (
   <div>
      {core.isLoading && <LoadingBar />}
      {core.failure.length > 0 && <MessageFailure>{core.failure}</MessageFailure>}

      {core.isOAuthApps && <Account />}
      {core.isOAuthAppRegister && <OAuthAppRegister />}
      {core.isOAuthAppCredentials && <OAuthAppSecret />}
   </div>   
)

const App = ({core, actions}) => (
   <Container>
      <Tab />
      <TabElement />
      <Core core={core} />
   </Container>
);

// const App = ({core, access_token, actions}) => (
//    <Container>
//       {access_token.error 
//          ? <Failure error={access_token.error} onClick={actions.signout} />
//          : <Core core={core} />
//       }
//    </Container>
// );

//
// Visual Map 
const vm = (state) => (state)
const dm = (dispatch) => ({actions: bindActionCreators({signout}, dispatch)})

export default connect(vm, dm)(App)
