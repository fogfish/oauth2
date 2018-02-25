import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Link } from 'react-dress-code'

const GitHub = ({client_id}) => (
   <Link href={`${window.env.GITHUB}&state=${client_id}`}>
      <i className="fa fa-github"></i> GitHub
   </Link>
)

const model = state => (state.core.authRequest)
const actions = dispatch => bindActionCreators({}, dispatch)
export default connect(model, actions)(GitHub)
