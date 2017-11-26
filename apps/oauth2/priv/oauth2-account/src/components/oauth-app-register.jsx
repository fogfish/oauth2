import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import {DialogWithAccept} from './dress-code';
import {appsview, credentials} from '../ducks/core'
import {onAppName, onAppSecurity, onAppRedirectUri, registerOAuthApp} from '../ducks/oauth-app'


const AppName = ({onChange}) => (
   <div>
      <label className="dc-label">
         Name <span className="dc-label__sub">required</span>
      </label>
      <input type="input" className="dc-input" autoFocus="true" required="required" onChange={onChange} />   
   </div>
)

const AppRedirectUri = ({onChange}) => (
   <div>
      <label className="dc-label">
         Redirect Uri<span className="dc-label__sub">required</span>
      </label>
      <input type="input" className="dc-input" required="required" onChange={onChange} />
   </div>
)

const AppType = ({onChange}) => (
   <div>
      <label className="dc-label">
         Application Type
      </label>
      <select className="dc-select dc-select--small" onChange={onChange}>
         <option value="public">public</option>
         <option value="confidential">confidential</option>
      </select>
   </div>
)

//
//
export const OAuthAppRegister = ({actions}) => (
   <DialogWithAccept
      title="Register"
      subtitle="New OAuth App"
      accept={actions.registerOAuthApp}
      cancel={actions.appsview}
   >
      <AppName onChange={actions.onAppName}/>
      <AppRedirectUri onChange={actions.onAppRedirectUri}/>
      <AppType onChange={actions.onAppSecurity}/>
   </DialogWithAccept>
)



//
// Visual Map 
const vm = (state) => (state)
const dm = (dispatch) => ({actions: bindActionCreators({appsview, registerOAuthApp, onAppName, onAppSecurity, onAppRedirectUri}, dispatch)})

export default connect(vm, dm)(OAuthAppRegister)
