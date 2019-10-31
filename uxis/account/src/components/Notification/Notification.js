import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { reset } from './ducks'
import './style.css'


const Failure = ({error, reset}) => (
  <div className="dc-msg dc-msg--error">
    <div className="dc-msg__inner">
      <div className="dc-msg__icon-frame">
        <i className="dc-icon dc-msg__icon dc-icon--error"></i>
      </div>

      <div className="dc-msg__bd">
        <h1 className="dc-msg__title">Error: {error.reason}</h1>
        <p className="dc-msg__text">{error.description}</p>
      </div>

      <div class="dc-msg__close" onClick={reset}>
        <i class="dc-icon dc-icon--close dc-msg__close__icon"></i>
      </div>
    </div>
  </div>
)


const Notification = (props) => (
  <div>
    {props.error && <Failure { ...props } />}
  </div>
)


const model = state => (state.notification)
const actions = dispatch => bindActionCreators({ reset }, dispatch)
export default connect(model, actions)(Notification)

