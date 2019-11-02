import React from 'react'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button } from 'react-dress-code'
import Identity from './Identity'
import Endpoint from './Endpoint'
import Security from './Security'

import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { commit } from '../ducks'

const Registrar = ({history, commit}) => (
  <Dialog>
    <DialogContent>
      <DialogBody>
        <DialogTitle>Register</DialogTitle>
        <DialogSubTitle>New OAuth App</DialogSubTitle>

        <Identity />
        <Endpoint />
        <Security />

      </DialogBody>
      <DialogActions with-link>
        <Button link onClick={ history.goBack }>Cancel</Button>
        <Button primary onClick={ commit }>Register</Button>
      </DialogActions>
    </DialogContent>
  </Dialog>
)

const model = state => (state.app)
const actions = dispatch => bindActionCreators({ commit }, dispatch)
export default connect(model, actions)(Registrar)
