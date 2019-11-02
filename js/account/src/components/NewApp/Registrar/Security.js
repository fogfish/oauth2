import React from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Label, Select } from 'react-dress-code'
import { security } from '../ducks'

const Security = ({ security }) => (
   <div>
      <Label sub="required">Application Type</Label>
      <Select small onChange={security}>
         <option value="public">public</option>
         <option value="confidential">confidential</option>
      </Select>
   </div>
)

const model = state => (state.app)
const actions = dispatch => bindActionCreators({ security }, dispatch)
export default connect(model, actions)(Security)

