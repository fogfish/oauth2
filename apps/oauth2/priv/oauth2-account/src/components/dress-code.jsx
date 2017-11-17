import React from 'react';

//
//
export const Container = ({children}) => (
   <div className="dc-page">
      <div className="dc-container dc-container-limited">
         {children}
      </div>
   </div>
)

export const Layout = ({children}) => (
   <div className="dc-row">
      {children}
   </div>
)

export const Tabs = ({title, children}) => (
   <div className="dc-column dc-column--large-2 dc-column--medium-2">
      <div className="dc-column__contents">
         <ul class="dc-tab dc-tab--vertical">
            <li className="dc-tab__element dc-tab__element--disabled dc-tab__element--header">{title}</li>
            {children}
         </ul>
      </div>
   </div>
)

export const View = ({children}) => (
   <div className="dc-column dc-column--large-10 dc-column--medium-10">
      <div className="dc-column__contents">
         {children}
      </div>
   </div>
)

//
//
export const Window = ({children}) => (
   <div className="dc-dialog">
      <form className="form-group">
         {children}
      </form>
   </div>  
)

export const Dialog = ({title, subtitle, children}) => (
   <div className="dc-dialog__content">
      <div className="dc-dialog__body">
         <h3 className="dc-dialog__title">{title}</h3>
         <h4 className="dc-dialog__subtitle">{subtitle}</h4>
         {children}
      </div>
   </div>
)


export const DialogWithOk = ({title, subtitle, children}) => (
   <Window>
      <Dialog title={title} subtitle={subtitle}>
         {children}
      </Dialog>
      <div className="dc-dialog__actions">
         <button className="dc-btn dc-btn--primary">OK</button>
      </div>
   </Window>
)

export const DialogWithAccept = ({title, subtitle, accept, children}) => (
   <Window>
      <Dialog title={title} subtitle={subtitle}>
         {children}
      </Dialog>
      <div className="dc-dialog__actions">
         <button className="dc-btn dc-btn--link">Cancel</button>
         <button className="dc-btn dc-btn--primary">{accept}</button>
      </div>
   </Window>
)


//
//
export const Tab = ({title}) => (
   <li className="dc-tab__element dc-tab__element--active">{title}</li>
)

