import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { KeyPair } from './KeyPair'
import { Registrar } from './Registrar' 

const NewApp = (props) => (
   <div>
      {props.keys ? <KeyPair { ...props } /> : <Registrar { ...props } />}
   </div>
)

const model = state => (state.app)
const actions = dispatch => bindActionCreators({}, dispatch)
export default connect(model, actions)(NewApp)
