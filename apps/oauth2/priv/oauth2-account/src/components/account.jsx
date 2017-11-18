import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import {Layout, Tabs, Tab, View} from './dress-code';
import {OAuthAppList} from './oauth-app';

import {register} from '../ducks/core';
import {signout} from '../ducks/access-token';


const OAuthApps = ({actions, children}) => (
   <div className="dc-card">
      <h2 className="dc-h2">
         OAuth Apps
         <button className="dc-btn dc-btn--link dc-btn--small" onClick={actions.register}>New OAuth App</button>
      </h2>

      {children
      ?  <OAuthAppList>{children}</OAuthAppList>
      :  <div>
            <h4 className="dc-h4">No OAuth applications...</h4>
            <p className="dc-p">OAuth applications are used to access REST API.</p>
            <a className="dc-btn dc-btn--primary" onClick={actions.register}>Register a new Application</a>
         </div>
      }
   </div>
)

const Account = ({actions}) => (
   <Layout signout={actions.signout}>
      <Tabs title="Developer Settings">
         <Tab title="OAuth Apps" />
      </Tabs>
      <View>
         <OAuthApps actions={actions}>
         </OAuthApps>
      </View>
   </Layout>
)


//
// Visual Map 
const vm = (state) => (state)
const dm = (dispatch) => ({actions: bindActionCreators({register, signout}, dispatch)})

export default connect(vm, dm)(Account)
