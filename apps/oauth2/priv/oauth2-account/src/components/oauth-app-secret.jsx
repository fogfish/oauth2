import React from 'react';
import {bindActionCreators} from 'redux';
import {connect} from 'react-redux';

import {DialogWithOk} from './dress-code';
import {appsview} from '../ducks/core'


const AppKey = ({title, value}) => (
   <div>
      <label className="dc-label">{title}</label>
      <p>{value}</p>
   </div>
)

//
//
const OAuthAppSecret = ({actions}) => (
   <DialogWithOk title="Credentials" subtitle="for OAuth App" accept={actions.appsview}>
      <p>&nbsp;</p>
      <AppKey title="Access Key" value="xxx" />
      <AppKey title="Secret Key" value="xxx" />
   </DialogWithOk>
)

//
// Visual Map 
const vm = (state) => (state)
const dm = (dispatch) => ({actions: bindActionCreators({appsview}, dispatch)})

export default connect(vm, dm)(OAuthAppSecret)
