import React from 'react'
import { Button, Intent, Label, Classes } from '@blueprintjs/core'
import { SecretKey } from 'components/SecretKey'
import { Dialog } from 'components/Dialog'

const Actions = () => (
  <>
    <>&nbsp;</>
    <Button type="submit" intent={Intent.PRIMARY} large>Reset Password</Button>
  </>
)

export const SecretRecover = ({ client_id, code, access }) => (
  <Dialog
    icon="fa-key"
    title="Reset Password"
    url="/oauth2/token"
    Actions={Actions}
  >
    <p className="bp3-ui-text bp3-running-text">
      Create a new password for <b>{access}</b>
    </p>
    <Label>
      Password&nbsp;<span className="bp3-text-muted">required</span>
      <input
        className={Classes.INPUT}
        id="password"
        name="password"
        type="password"
        required
      />
    </Label>
    <input name="grant_type" type="hidden" value="password_update" />
    <input name="client_id" type="hidden" value={client_id} />
    <input name="code" type="hidden" value={code} />
  </Dialog>
)
