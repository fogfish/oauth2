import React from 'react'
import {
  Button,
  Classes,
  Intent,
  Callout,
  Code,
} from '@blueprintjs/core'
import { PENDING, FAILURE } from 'react-hook-oauth2'
import Identity from './Identity'
import Endpoint from './Endpoint'
import Security from './Security'

const Registrar = ({
  status,
  commit,
  app,
  update,
}) => (
  <>
    <div className={Classes.DIALOG_BODY}>
      <p>
        Registers the application with the authorization server. Define client type
        based on their ability to maintain the confidentiality of their client credentials.
        The authorization server issues the registered client a client identifier and client secret.
      </p>

      {(status instanceof FAILURE && status.reason.details !== 'invalid_uri')
        && (
          <Callout intent={Intent.DANGER}>
            Unable to register an application. Try again later.
          </Callout>
        )}
      <Identity app={app} update={update} />

      {(status instanceof FAILURE && status.reason.details === 'invalid_uri')
        && (
          <Callout intent={Intent.DANGER}>
            Invalid&nbsp;
            <strong>Redirect Uri</strong>
            .&nbsp;The schema, host and path are required.
            <Code>https://example.com/path</Code>
          </Callout>
        )}
      <Endpoint app={app} update={update} />
      <Security app={app} update={update} />
    </div>
    <div className={Classes.DIALOG_FOOTER}>
      <div className={Classes.DIALOG_FOOTER_ACTIONS}>
        <Button
          intent={Intent.PRIMARY}
          onClick={commit}
          loading={status instanceof PENDING}
        >
          Register
        </Button>
      </div>
    </div>
  </>
)

export default Registrar
