import React from 'react';
import {Layout, Tabs, Tab, View} from './dress-code';
import {OAuthAppList, OAuthApp} from './oauth-app'


const OAuthApps = ({children}) => (
   <div className="dc-card">
      <h2 className="dc-h2">
         OAuth Apps
         <a href="#" className="dc-btn dc-btn--link dc-btn--small">New OAuth App</a>
      </h2>

      {children
      ?  <OAuthAppList>{children}</OAuthAppList>
      :  <div>
            <h4 className="dc-h4">No OAuth applications</h4>
            <p className="dc-p">OAuth applications are used to access REST API.</p>
            <a className="dc-btn dc-btn--primary">Register a new Application</a>
         </div>
      }
   </div>
)

export const Account = () => (
   <Layout>
      <Tabs title="Developer Settings">
         <Tab title="OAuth Apps" />
      </Tabs>
      <View>
         <OAuthApps>
            <OAuthApp />
         </OAuthApps>
      </View>
   </Layout>
)