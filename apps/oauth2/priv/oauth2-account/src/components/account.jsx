import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import {Layout, Tabs, Tab, View} from './dress-code';
import {OAuthAppList, OAuthApp} from './oauth-app';

import {showRegisterNewApp} from '../ducks/core';
import {signout} from '../ducks/access-token';
import {revokeOAuthApp} from '../ducks/oauth-app';


const OAuthApps = ({actions, apps}) => (
   <div className="dc-card">
      <h2 className="dc-h2">
         OAuth Apps
         <button className="dc-btn dc-btn--link dc-btn--small" onClick={actions.showRegisterNewApp}>New OAuth App</button>
      </h2>

      {apps.length > 0 &&
         <OAuthAppList>
            {apps.map(x => <OAuthApp app={x} onRevoke={actions.revokeOAuthApp}/>)}
         </OAuthAppList>
      }
      {apps.length == 0 &&
         <div>
            <h4 className="dc-h4">No OAuth applications...</h4>
            <p className="dc-p">OAuth applications are used to access REST API.</p>
            <a className="dc-btn dc-btn--primary" onClick={actions.showRegisterNewApp}>Register a new Application</a>
         </div>
      }
   </div>
)

const Account = ({oauthApps, actions}) => (
   <Layout signout={actions.signout}>
      <Tabs title="Developer Settings">
         <Tab title="OAuth Apps" />
      </Tabs>
      <View>
         <OAuthApps actions={actions} apps={oauthApps} />
      </View>
   </Layout>
)


//
// Visual Map 
const vm = (state) => (state.account)
const dm = (dispatch) => ({actions: bindActionCreators({revokeOAuthApp, showRegisterNewApp, signout}, dispatch)})

export default connect(vm, dm)(Account)
