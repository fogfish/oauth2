import React from 'react'
import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'
import { requireAuth, authorize } from './ducks'



const OAuth2 = props => (
  <div className="oauth2-container">
    {!props.exchange && props.error === null &&
      (props.requireAuth() ? props.authorize() : props.children)
    }
    {props.error && `TODO: Handle OAuth2 Error: ${props.error}`}
  </div>
)

const model = state => (state.oauth2)
const actions = dispatch => bindActionCreators({ requireAuth, authorize }, dispatch)
export default connect(model, actions)(OAuth2)
