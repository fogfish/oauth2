import React from 'react'
import { Button, Intent } from '@blueprintjs/core'
import { Link } from 'react-router-dom'
import Dialog from 'components/Dialog'
import AccessKey from 'components/AccessKey'

const Actions = () => (
  <>
    <Link className="bp3-button bp3-minimal bp3-intent-primary" to="/">
      <b>Sign In Instead</b>
    </Link>
    <Button type="submit" intent={Intent.PRIMARY} large>Reset Password</Button>
  </>
)

const SecretReset = ({ oauth2 }) => (
  <Dialog
    icon="fa-lock"
    title="Forgot Your Password ?"
    url="/oauth2/password"
    Actions={Actions}
  >
    <p className="bp3-ui-text bp3-running-text">
      Type you&nbsp;
      <b>email</b>
      &nbsp;address to reset your password.
      We will send recovery instructions over email.
    </p>
    <AccessKey />

    <input name="response_type" type="hidden" value="password_reset" />
    <input name="client_id" type="hidden" value={oauth2.clientId} />
    <input name="redirect_uri" type="hidden" value={oauth2.redirectUri} />
  </Dialog>
)

export default SecretReset
