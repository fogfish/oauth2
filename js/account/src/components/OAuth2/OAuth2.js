import React from 'react'
import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'
import { requireAuth, authorize } from './ducks'
import { LoadingBar } from 'react-dress-code'

const Failure  = (props) => (
  <div className="dc-msg dc-msg--error">
    <div className="dc-msg__inner">
      <div className="dc-msg__icon-frame">
        <i className="dc-icon dc-msg__icon dc-icon--error"></i>
      </div>

      <div className="dc-msg__bd">
        <h1 className="dc-msg__title">Error: {props.error}</h1>
        <p className="dc-msg__text">
          Unable to authenticate! <a href class="dc-link dc-msg__bd__link" onClick={props.authorize}>Please try again!</a> 
        </p>
      </div>

    </div>
  </div>   
)

const OAuth2 = props => (<>{props.children}</>)

  // <div className="oauth2-container">
  //   {props.exchange && <LoadingBar />}
  //   {!props.exchange && props.error === null &&
  //     (props.requireAuth() ? props.authorize() : props.children)
  //   }
  //   {props.error && <Failure {...props} />}
  // </div>



const model = state => (state.oauth2)
const actions = dispatch => bindActionCreators({ requireAuth, authorize }, dispatch)
export default connect(model, actions)(OAuth2)
