import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import {DialogWithOk, MessageSuccess} from './dress-code';
import {appsview} from '../ducks/core'


const AppKey = ({title, value}) => (
   <div>
      <label className="dc-label">{title}</label>
      <p className="oauth-key">{value}</p>
   </div>
)

//
//
const OAuthAppSecret = ({access, secret, actions}) => (
   <DialogWithOk title="Credentials" subtitle="for OAuth App" accept={actions.appsview}>
      <MessageSuccess>
         This is the <strong>only</strong> time that the secret access keys can be viewed or downloaded. You cannot recover them later. However, you can create new access keys at any time.
      </MessageSuccess>

      <AppKey title="Access Key" value={access} />
      <AppKey title="Secret Key" value={secret} />
   </DialogWithOk>
)

//
// Visual Map 
const vm = (state) => (state.app.keys)
const dm = (dispatch) => ({actions: bindActionCreators({appsview}, dispatch)})

export default connect(vm, dm)(OAuthAppSecret)
