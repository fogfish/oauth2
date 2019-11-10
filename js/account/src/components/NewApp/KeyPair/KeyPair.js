import React from 'react'
import { Button, Classes, Callout, Label, Code, Dialog, H5, Intent, Switch, Tooltip } from "@blueprintjs/core";

/*
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'
import { Dialog, DialogContent, DialogBody, DialogTitle, DialogSubTitle, DialogActions, Button, Label } from 'react-dress-code'
import { register } from '../ducks'
*/

const Secret = () => (
  <>
      <div className={Classes.DIALOG_BODY}>
        <Callout intent={Intent.WARNING}>
          This is the <strong>only</strong> time that the secret access keys can be viewed or downloaded. 
          You cannot recover them later. However, you can create new access keys at any time.
        </Callout>

        <Callout style={{marginTop: '1em'}}>
          <H5>Access Key</H5>
          <p><Code>Access Key</Code></p>

          <H5>Secret Key</H5>
          <p><Code>Access Key</Code></p>
        </Callout>

      </div>
      <div className={Classes.DIALOG_FOOTER}>
          <div className={Classes.DIALOG_FOOTER_ACTIONS}>
              <Button
                intent={Intent.PRIMARY}
              >
                Ok
              </Button>
          </div>
      </div>
  </>
)

export default Secret

/*
const Secret = ({ register, history, keys }) => (
  <Dialog>
    <DialogContent>
      <DialogBody>
        <DialogTitle>Credentials</DialogTitle>
        <DialogSubTitle>for OAuth App</DialogSubTitle>

        <div className="dc-msg dc-msg--success">
          <div className="dc-msg__inner">
            <div className="dc-msg__icon-frame">
              <i className="dc-icon dc-msg__icon dc-icon--success"></i>
            </div>
            <div className="dc-msg__bd">
              <h1 className="dc-msg__title">Success</h1>
              <p className="dc-msg__text">
                This is the <strong>only</strong> time that the secret access keys can be viewed or downloaded. 
                You cannot recover them later. However, you can create new access keys at any time.
              </p>
            </div>
          </div>
        </div>

        <Label>Access Key</Label>
        <p className="oauth-key">{keys.access}</p>

        <Label>Secret Key</Label>
        <p className="oauth-key">{keys.secret}</p>

      </DialogBody>
      <DialogActions with-link>
        <Button primary onClick={() => register(history)}>Ok</Button>
      </DialogActions>
    </DialogContent>
  </Dialog>
)

const model = state => (state.app)
const actions = dispatch => bindActionCreators({ register }, dispatch)
export default connect(model, actions)(Secret)
*/
