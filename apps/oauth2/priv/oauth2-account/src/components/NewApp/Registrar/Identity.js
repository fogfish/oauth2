import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Label, Input } from 'react-dress-code'
import { identity } from '../ducks'

const Identity = ({ identity }) => (
   <div>
      <Label sub="required">Application</Label>
      <Input 
         type="input"
         autoFocus
         required
         onChange={identity} />
   </div>
)

const model = state => (state.app)
const actions = dispatch => bindActionCreators({ identity }, dispatch)
export default connect(model, actions)(Identity)
