import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Label, Input } from 'react-dress-code'
import { endpoint } from '../ducks'

const Endpoint = ({ endpoint }) => (
   <div>
      <Label sub="required">Redirect Uri</Label>
      <Input 
         type="input"
         required
         onChange={endpoint} />
   </div>
)

const model = state => (state.app)
const actions = dispatch => bindActionCreators({ endpoint }, dispatch)
export default connect(model, actions)(Endpoint)

