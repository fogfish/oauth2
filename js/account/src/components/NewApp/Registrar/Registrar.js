import React from 'react'
import { Button, Classes, Code, Dialog, H5, Intent, Switch, Tooltip } from "@blueprintjs/core";

// import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button } from 'react-dress-code'
import Identity from './Identity'
import Endpoint from './Endpoint'
import Security from './Security'

/*
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { commit } from '../ducks'
*/

const Registrar = (props) => (
  <>
      <div className={Classes.DIALOG_BODY}>
        <p>
          Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus a egestas dui. 
          Fusce nec egestas risus, tincidunt lobortis ligula. Nulla finibus felis consequat 
          turpis tincidunt tincidunt. Etiam facilisis ex in arcu egestas malesuada. Suspendisse 
          viverra dui vestibulum tristique bibendum. Sed tincidunt leo nec purus finibus volutpat. 
          Sed porttitor risus lacus, quis semper nibh sodales nec.
        </p>
        <Identity { ...props } />
        <Endpoint { ...props } />
        <Security { ...props } />
      </div>
      <div className={Classes.DIALOG_FOOTER}>
          <div className={Classes.DIALOG_FOOTER_ACTIONS}>
              <Button
                intent={Intent.PRIMARY}
                onClick={props.commit}
              >
                Register
              </Button>
          </div>
      </div>
  </>
)

/*
const Registrar1 = ({history, commit}) => (
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
*/

// const model = state => (state.app)
// const actions = dispatch => bindActionCreators({ commit }, dispatch)
export default Registrar //connect(model, actions)(Registrar)
